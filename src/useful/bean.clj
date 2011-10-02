(ns useful.bean
  "Modify bean attributes in clojure."
  (:import [java.beans Introspector]))

(defn- property-key [property]
  (keyword (.. (re-matcher #"\B([A-Z])" (.getName property))
               (replaceAll "-$1")
               toLowerCase)))

(defn property-setters
  "Returns a map of keywords to property setter methods for a given class."
  [class]
  (reduce
   (fn [map property]
     (assoc map (property-key property) (.getWriteMethod property)))
   {} (.getPropertyDescriptors (Introspector/getBeanInfo class))))

(defmulti coerce (fn [bean-class type val] [type (class val)]))
(defmethod coerce :default [_ type val]
  (if (= String type)
    (str val)
    (try (cast type val)
         (catch ClassCastException e
           val))))

(defn update-bean
  "Update the given bean instance with attrs by calling the appropriate setter methods on it."
  [instance attrs]
  (let [bean-class (class instance)
        setters    (property-setters bean-class)]
    (doseq [[key val] attrs]
      (if-let [setter (setters key)]
        (when-not (nil? val)
          (let [type (first (.getParameterTypes setter))]
            (.invoke setter instance (into-array [(coerce bean-class type val)]))))
        (throw (Exception. (str "property not found for " key)))))
    instance))
