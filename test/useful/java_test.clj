(ns useful.java-test
  (:use clojure.test useful.java))

(deftest test-rescue
  (is (= nil (rescue (/ 9 0) nil)))
  (is (= 3   (rescue (/ 9 3) nil))))
