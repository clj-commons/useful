(ns useful.cli-test
  (:use clojure.test useful.cli))

(deftest test-parse-opts
  (is (= {:foo ["a"] :bar [""]} (parse-opts ["--foo=a" "--bar"]))))
