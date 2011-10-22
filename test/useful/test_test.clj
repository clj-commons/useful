(ns useful.test-test
  (:use useful.test clojure.test))

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


;; defines a test with no tags attached:
(deftest plain-deftest
  (is (not (contains? (meta #'plain-deftest) :tags))))

(with-test-tags [foo]

  ;; this test will be tagged #{:foo}:
  (deftest foo
    (tags? foo [foo]))

  (with-test-tags [bar]

    ;; this test will be tagged #{:foo :bar}:
    (deftest foo-bar
      (tags? foo-bar [foo bar]))))

;; tests inside with-test-args can be closures:
(with-test-tags [foo]
  (let [x #{:foo}]
    (deftest lexical-bindings-with-tags
      (is (= x (:tags (meta #'lexical-bindings-with-tags)))))))
