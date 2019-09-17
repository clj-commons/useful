(ns flatland._bootstrap
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str] ))

(defn print-versions []
  (let [version-str (format "   Clojure %s    Java %s"
                      (clojure-version) (System/getProperty "java.version"))
        num-hyphen  (+ 6 (count version-str))
        hyphens     (str/join (repeat num-hyphen \-)) ]
    (newline)
    (println hyphens)
    (println version-str)
    (println hyphens)))

(deftest t-bootstrap-16
  (print-versions))
