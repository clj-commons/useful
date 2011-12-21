(ns useful.string
  (:require [clojure.string :as s]))

(defn camelize [string]
  (s/replace string
             #"[-_](\w)"
             (comp s/upper-case second)))

(defn classify [string]
  (apply str (map s/capitalize
                  (s/split string #"[-_]"))))

(defn- from-camel-fn [separator]
  (fn [string]
    (-> string
        (s/replace #"^[A-Z]+" s/lower-case)
        (s/replace #"_?([A-Z]+)"
                   (comp (partial str separator)
                         s/lower-case second))
        (s/replace #"-|_" separator))))

(def dasherize (from-camel-fn "-"))
(def underscore (from-camel-fn "_"))

(defn pluralize
  "Return a pluralized phrase, appending an s to the singular form if no plural is provided.
  For example:
     (plural 5 \"month\") => \"5 months\"
     (plural 1 \"month\") => \"1 month\"
     (plural 1 \"radius\" \"radii\") => \"1 radius\"
     (plural 9 \"radius\" \"radii\") => \"9 radii\""
  [num singular & [plural]]
  (str num " " (if (= 1 num) singular (or plural (str singular "s")))))
