(ns useful.dispatch
  (:use [useful.map :only [into-map]]
        [useful.fn :only [any]]))

(defn dispatcher
  "Returns a function that dispatches using the given dispatch function to determine the
  namespace and function to call."
  [dispatch-fn & options]
  (let [{:keys [hierarchy wrap default] :or {wrap identity}} (into-map options)]
    (fn [& args]
      (let [fname   (apply dispatch-fn args)
            default (or default
                        (with-meta (fn [& args]
                                     (throw (IllegalArgumentException. (str "cannot resolve function: " fname))))
                          {:no-wrap true}))]
        (loop [[ns method] (map symbol ((juxt namespace name) (symbol fname)))]
          (if-let [f (if ns
                       (try (require ns)
                            (ns-resolve ns method)
                            (catch java.io.FileNotFoundException e))
                       default)]
            (let [wrap (if (not (:no-wrap (meta f))) wrap identity)]
              (apply (wrap f) args))
            (recur [(get hierarchy ns) method])))))))

(defmacro defdispatch
  "Defines a function that dispatches using the given dispatch function to determine the
  namespace and function to call."
  {:arglists '([name docstring? attr-map? dispatch-fn & options])}
  [name & options]
  (let [[defn-options [dispatch-fn & options]] (split-with (any string? map?) options)]
    `(let [dispatcher# (dispatcher ~dispatch-fn ~@options)]
       (defn ~name ~@defn-options [& args#]
         (apply dispatcher# args#)))))