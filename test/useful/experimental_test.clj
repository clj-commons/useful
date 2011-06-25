(ns useful.experimental-test
  (:use clojure.test useful.experimental))

(deftest test-while-let
  (let [a (atom '(1 2 3 4 5))]
    (while-let [val (seq @a)]
      (is val)
      (swap! a rest))
    (is (empty? @a))))

(deftest test-let-if
  (doseq [a [1 2]]
    (let-if (even? a)
            [odd  false true
             even true  false]
      (is (= even (even? a)))
      (is (= odd  (odd?  a))))))