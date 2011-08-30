(ns useful.test-test
  (:use useful.test clojure.test clojure.tools.macro))

(defmacro tags? [test expected]
  `(is (= ~(set (map keyword expected))
          (-> ~test var meta :tags))))

(with-test-tags [unit]
  (deftest test-unit
    (tags? test-unit [unit]))
  (with-test-tags [debug]
    (deftest test-debug
      (tags? test-debug [unit debug])))
  (deftest more-unit-tests
    (tags? more-unit-tests [unit])))
