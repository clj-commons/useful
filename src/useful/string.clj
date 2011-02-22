(ns useful.string
  (:use [clojure.string :only [join split capitalize]]))

(defn camelize [s & [lower]]
  (let [parts (split (str s) #"-|_")]
    (apply str
           (if lower
             (cons (first parts) (map capitalize (rest parts)))
             (map capitalize parts)))))

(defn dasherize [s]
  (.. (re-matcher #"\B([A-Z])" (str s))
      (replaceAll "-$1")
      toLowerCase))

(defn underscore [s]
  (.. (re-matcher #"\B([A-Z])" (str s))
      (replaceAll "_$1")
      toLowerCase))