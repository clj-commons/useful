(ns useful.amalloy.transform)

(defn transform-if
  "Returns a function that tests pred against its argument. If the result
is true, return (f arg); otherwise, return (f-not arg) (defaults to
identity)."
  ([pred f]
     (fn [x]
       (if (pred x) (f x) x)))
  ([pred f f-not]
     (fn [x]
       ((if (pred x) f f-not) x))))

(defmacro with-adjustments
  "Create new bindings for binding args, by applying adjustment
  function to current values of bindings."
  [adjustment bindings & body]
  (let [bindings (vec bindings)]
    `(let [~bindings (map ~adjustment ~bindings)]
       ~@body)))
