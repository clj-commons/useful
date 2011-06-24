(ns useful.fn-test
  (:use clojure.test useful.fn))

(deftest test-decorate
  (is (= [[1 2] [2 3] [3 4]] (map (decorate inc) [1 2 3]))))

(deftest test-annotate
  (is (= [1 2] (annotate inc 1))))

(deftest test-transform-if
  (is (= [1 -2 3 -4] (map (transform-if even? -) [1 2 3 4]))))

(deftest test-any
  (is (= [0 2 3 4 6 8 9 10]
         (filter (any #(zero? (rem % 2))
                      #(zero? (rem % 3)))
                 (range 11)))))

(deftest test-all
  (is (= [0 6]
         (filter (all #(zero? (rem % 2))
                      #(zero? (rem % 3)))
                 (range 11)))))

(deftest test-thrush
  (is (= 5 (thrush 1 inc inc inc inc))))
