(ns useful.state
  (:import [clojure.lang IDeref IObj]))

(defprotocol Mutable
  (put! [self v]))

(deftype Volatile [^{:volatile-mutable true} val validator meta]
  IDeref
  (deref [self] val)
  Mutable
  (put! [self v]
    (if (and validator (not (validator v)))
      (throw (IllegalStateException. "Invalid reference state"))
      (set! val v)))
  IObj
  (meta [self]
    meta)
  (withMeta [self meta]
    (Volatile. val validator meta)))

(defn volatile
  "Creates and returns a Volatile with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will become the metadata on the
  Volatile. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception."
  ([x]
     (Volatile. x nil {}))
  ([x & options]
     (let [opts (apply hash-map options)]
       (Volatile. x (:validator opts) (:meta opts)))))

(defn trade!
  "Like swap!, except it returns the old value of the atom."
  [atom f & args]
  (let [m (volatile nil)]
    (apply swap! atom
           (fn [val & args]
             (put! m val)
             (apply f val args))
           args)
    @m))

(defn wait-until [reference pred]
  (let [curr @reference] ;; try to get out fast - not needed for correctness, just performance
    (if (pred curr)
      curr
      (let [result (promise)]
        (add-watch reference result
                   (fn this [_ _ old new]
                     (when (pred new)
                       (try ;; multiple delivers throw an exception in clojure 1.2
                         (when (deliver result new)
                           (remove-watch reference result))
                         (catch Exception e
                           nil)))))
        (let [curr @reference] ; needed for correctness, in case it's become acceptable since adding
                               ; watcher and will never change again
          (if (pred curr)
            (do (remove-watch reference result)
                curr)
            @result))))))
