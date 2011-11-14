(ns useful.io-test
  (:use useful.io clojure.test))

(deftest test-read-seq
  (let [forms '(this (is) #(100 %) ~of a [long, [complicated, [nested]]] {:quoted #{form}})
        form-str (with-out-str (doseq [form forms]
                                 (prn form)))]
    (is (= forms (read-seq (java.io.StringReader. form-str))))))
