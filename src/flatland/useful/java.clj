(ns flatland.useful.java
  (:import (java.lang.reflect Method)))

(defn ^{:dont-test "Can't test killing the JVM"} abort
  "Print message then exit."
  [& message]
  (apply println message)
  (System/exit 1))

(defn ^{:dont-test "Can't send a signal in order to catch it!"} trap
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
        c (class instance)]
    (when-let [^Method method (some #(try
                                       (.getDeclaredMethod ^Class % method signature)
                                       (catch NoSuchMethodException e))
                                    (conj (ancestors c) c))]
      (let [accessible (.isAccessible method)]
        (.setAccessible method true)
        (let [result (.invoke method instance (into-array params))]
          (.setAccessible method accessible)
          result)))))

(defn ^{:dont-test "Can't test shutting down JVM"} on-shutdown
  "Execute the given function on jvm shutdown."
  [^Runnable f]
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. f)))

(defmacro multi-hinted-let
  "Test expr for instance-of each class in classes. When a match is found,
   evaluate body with name bound to expr and type-hinted as the matching class.

   For example, (multi-hinted-let [x {:foo 1} [Collection Map]] (.size x))."
  [[name expr classes] & body]
  (let [x (gensym)]
    `(let [~x ~expr]
       (condp instance? ~x
         ~@(interleave classes
                       (for [class classes]
                         `(let [~(vary-meta name assoc :tag class) ~x] ~@body)))
         (throw (IllegalArgumentException. (str "No matching class for " ~x " in " '~classes)))))))
