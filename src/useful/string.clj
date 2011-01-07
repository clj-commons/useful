(ns useful.string
  (:use [clojure.string :only [join split capitalize]]))

(defn camelize [str]
  (s/replace str
             #"-(\w)"
             (comp s/upper-case second))[s & [lower]])

(defn dasherize [name]
  (s/replace name
             #"(?<![A-Z])[A-Z]+"
             (comp (partial str "-")
                   s/lower-case)))

(defn underscore [s]
  (.. (re-matcher #"\B([A-Z])" (str s))
      (replaceAll "_$1")
      toLowerCase))