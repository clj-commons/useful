(ns useful.seq-test
  (:use clojure.test useful.seq))

(deftest test-zip
  (is (= [[1 4 8] [2 5 9] [3 6 nil] [nil 7 nil]] (zip [1 2 3] [4 5 6 7] [8 9]))))

(deftest test-find-with
  (is (= :foo (find-with odd?  [2 4 5 7] [:bar :baz :foo :bap])))
  (is (= nil  (find-with even? [1 3 5 9] [:bar :baz :foo :bap]))))

(deftest test-cross
  (is (= '((0 0) (0 1) (1 0) (1 1))          (cross [0 1] [0 1])))
  (is (= '((0 0 2) (0 1 2) (1 0 2) (1 1 2))) (cross [0 1] [0 1] [2])))

(deftest test-lazy-cross
  (is (= '((0 0) (1 0) (0 1) (1 1))         (lazy-cross [0 1] [0 1])))
  (is (= '((0 0 2) (1 0 2) (0 1 2) (1 1 2)) (lazy-cross [0 1] [0 1] [2]))))

(deftest test-extract
  (is (= [5 '(2 4 6 1 2 7)] (extract odd?     [2 4 6 5 1 2 7])))
  (is (= [2 '(4 6 5 1 2 7)] (extract even?    [2 4 6 5 1 2 7])))
  (is (= [7 '(2 4 6 5 1 2)] (extract #(< 6 %) [2 4 6 5 1 2 7]))))

(deftest test-separate
  (is (= ['(5 1 7) '(2 4 6 2)] (separate odd?  [2 4 6 5 1 2 7])))
  (is (= ['(2 4 6 2) '(5 1 7)] (separate even? [2 4 6 5 1 2 7]))))
