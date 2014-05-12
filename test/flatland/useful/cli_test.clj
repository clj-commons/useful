(ns flatland.useful.cli-test
  (:use clojure.test flatland.useful.cli))

(deftest test-parse-opts
  (is (= {:foo ["a"] :bar [""]} (parse-opts ["--foo=a" "--bar"]))))
