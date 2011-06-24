(ns useful.parallel
  (:use [useful.seq :only [slice]]))

(def *pcollect-thread-num* (.. Runtime getRuntime availableProcessors))

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