(ns useful.bean
  "Modify bean attributes in clojure."
  (:require [clojure.string :as s])
  (:import (java.beans Introspector PropertyDescriptor)
           (java.lang.reflect Method)))

(defn- property-key [^PropertyDescriptor property]
  (keyword (-> property
               .getName
               (s/replace #"\B([A-Z])" "-$1")
               .toLowerCase)))

(defn property-setters
  "Returns a map of keywords to property setter methods for a given class."
  [class]
  (reduce
   (fn [map ^PropertyDescriptor property]
     (assoc map (property-key property) (.getWriteMethod property)))
   {} (.getPropertyDescriptors (Introspector/getBeanInfo class))))

(defmulti coerce (fn [bean-class type val] [type (class val)]))
(defmethod coerce :default [_ type val]
  (when-not (nil? val)
    (try (cast type val)
         (catch ClassCastException e
           val))))

(defn update-bean
  "Update the given bean instance with attrs by calling the appropriate setter methods on it."
  [instance attrs]
  (let [bean-class (class instance)
        setters    (property-setters bean-class)]
    (doseq [[key val] attrs]
      (if-let [^Method setter (setters key)]
        (let [type (first (.getParameterTypes setter))]
          (.invoke setter instance (to-array [(coerce bean-class type val)])))
        (throw (Exception. (str "property not found for " key)))))
    instance))
