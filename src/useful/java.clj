(ns useful.java)

(defn abort
  "Print message then exit."
  [& message]
  (apply println message)
  (System/exit 1))

(defmacro rescue ;; XXX Throwable?
  "Evaluate form, returning error-form on any Exception."
  [form error-form]
  `(try ~form (catch Exception e# ~error-form)))

(defmacro verify
  "Raise exception unless test returns true."
  [test exception]
  `(when-not ~test
     (throw (if (string? ~exception)
              (Exception. ~exception)
              ~exception))))

(defn trap
  "Register signal handling function."
  [signal f]
  (sun.misc.Signal/handle
   (sun.misc.Signal. signal)
   (proxy [sun.misc.SignalHandler] []
     (handle [sig] (f sig)))))

(defn construct
  "Construct a new instance of class using reflection."
  [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

(defn invoke-private
  "Invoke a private or protected Java method. Be very careful when using this!
   I take no responsibility for the trouble you get yourself into."
  [instance method & params]
  (let [signature (into-array Class (map class params))
        c (.getClass instance)]
    (when-let [method (some #(try
                               (.getDeclaredMethod % method signature)
                               (catch NoSuchMethodException e))
                            (conj (ancestors c) c))]
      (let [accessible (.isAccessible method)]
        (.setAccessible method true)
        (let [result (.invoke method instance (into-array params))]
          (.setAccessible method accessible)
          result)))))

(defn on-shutdown
  "Execute the given function on jvm shutdown."
  [f]
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. f)))