(ns useful.datatypes-test
  (:use clojure.test useful.datatypes))

(defrecord Test [a b c])
(record-accessors Test)

(deftest test-record
  (let [init (Test. 1 2 3)
        second (Test. 1 5 4)]
    (is (= init (make-record Test :b 2 :a 1 :c 3)))
    (is (= second (assoc-record init :b 5 :c 4)))
    (is (= second (update-record init (+ b 3) (inc c))))
    (is (= (:a init) (a init)))))
