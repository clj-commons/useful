(ns useful.string
  (:use [useful.amalloy.debug :only [?]])
  (:require [clojure.string :as s]))

(defn camelize [str]
  (s/replace str
             #"[-_](\w)"
             (comp s/upper-case second)))

(defn- from-camel-fn [separator]
  (fn [name]
    (-> name
        (s/replace #"^[A-Z]+" s/lower-case)
        (s/replace #"_?([A-Z]+)"
                   (comp (partial str separator)
                         s/lower-case second))
        (s/replace #"-|_" separator))))

(def dasherize (from-camel-fn "-"))
(def underscore (from-camel-fn "_"))
