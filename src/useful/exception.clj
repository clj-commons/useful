(ns useful.exception
  (:use [clojure.stacktrace :only [print-cause-trace]]
        [clojure.string :only [split-lines trim]]))

(defmacro rescue
  "Evaluate form, returning error-form on any Exception."
  [form error-form]
  `(try ~form (catch Exception e# ~error-form)))

(defn cause-trace
  "Return an Exception's cause trace as an array of lines"
  [exception]
  (map trim (split-lines (with-out-str (print-cause-trace exception)))))

(defn exception-map
  "Return a map with the keys: :name, :message, and :trace. :trace is the cause trace as an array of lines "
  [exception]
  {:name    (.getName (class exception))
   :message (.getMessage ^Throwable exception)
   :trace   (cause-trace exception)})
