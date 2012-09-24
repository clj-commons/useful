(ns useful.exception-test
  (:use clojure.test useful.exception))

(deftest test-rescue
  (is (= nil (rescue (/ 9 0) nil)))
  (is (= 3   (rescue (/ 9 3) nil))))
