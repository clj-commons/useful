(ns useful.datatypes-test
  (:use clojure.test useful.datatypes))

(defrecord Test [a b])
(record-accessors Test)

(deftest test-record
  (let [init (Test. 1 2)
        second (Test. 1 5)]
    (is (= init (make-record Test :b 2 :a 1)))
    (is (= second (assoc-record init :b 5)))
    (is (= second (update-record init (+ b 3))))
    (is (= (:a init) (a init)))))
