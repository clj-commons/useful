(ns useful-test
  (:use clojure.test useful))

(deftest test-into-map
  (is (= {:foo "1", :bar "2", :bang "3", :baz "4", :blah 5}
         (into-map :foo 1 :bar 2 :bang 3 [:foo "1" :baz "4"] :bar "2" '(:bang "3") {:blah 5}))))

(deftest test-extract
  (is (= [5 '(2 4 6 1 2 7)] (extract odd? [2 4 6 5 1 2 7]))))

(deftest test-separate
  (is (= ['(5 1 7) '(2 4 6 2)] (separate odd? [2 4 6 5 1 2 7]))))