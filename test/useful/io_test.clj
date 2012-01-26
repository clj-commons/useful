(ns useful.io-test
  (:use useful.io clojure.test)
  (:import (java.io StringReader)))

(deftest test-read-seq
  (let [forms '(this (is) #(100 %) ~of a [long, [complicated, [nested]]] {:quoted #{form}})
        form-str (with-out-str (doseq [form forms]
                                 (prn form)))]
    (is (= forms (read-seq (StringReader. form-str))))))

(deftest test-bytes-and-longs
  (are [x bs] (and (= x (bytes->long (into-array Byte/TYPE (map byte bs))))
                   (= bs (seq (long->bytes x))))
       10    [0 0 0 0 0 0 0 10]
       255   [0 0 0 0 0 0 0 -1]
       256   [0 0 0 0 0 0 1 0]
       65540 [0 0 0 0 0 1 0 4]))
