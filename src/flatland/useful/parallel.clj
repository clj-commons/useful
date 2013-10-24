(ns flatland.useful.parallel
  (:use [flatland.useful.seq :only [slice]]))

(def ^{:dynamic true} *pcollect-thread-num* (.. Runtime getRuntime availableProcessors))

(defn pcollect
  "Like pmap but not lazy and more efficient for less computationally intensive functions
   because there is less coordination overhead. The collection is sliced among the
   available processors and f is applied to each sub-collection in parallel using map."
  ([f coll]
     (pcollect identity f coll))
  ([wrap-fn f coll]
     (if (<= *pcollect-thread-num* 1)
       ((wrap-fn #(doall (map f coll))))
       (mapcat deref
               (map (fn [slice]
                      (let [body (wrap-fn #(doall (map f slice)))]
                        (future-call body)))
                    (slice *pcollect-thread-num* coll))))))


(defn- assoc-noclobber 
  "An assoc wrapper which ensures that existing keys will not be
  clobbered by subsequent assoc invocations. 

  Used as a helper for locking-memoize to ensure that (delay) refs
  cannot be lost by swap! retry behavior."
  
  [m k v]
  (if (contains? m k) m
      (assoc m k v)))

(defn pmemoize 
  "Memoizes the function f, using the same approach as
  clojure.core/memoize. The practical difference is that this function
  provides the gurantee that in spite of parallel invocations of the
  memoized function each input to f will only ever be memoized
  once. This resolves an implementation detail in clojure.core/memoize
  which allows f to be applied to args without locking the cache to
  prevent other threads duplicating the work."

  [f]
  (let [mem (atom {})]
    (fn [ & args ]
      (if-let [e (find @mem args)]
        (deref (val e))
        (-> (swap! mem assoc-noclobber
                   args (delay (apply f args)))
            (get args)
            (deref))))))
