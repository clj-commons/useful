(ns useful.ns-test
  (:use clojure.test useful.ns)
  (:require [useful.fn :as fn]
            [useful.macro :as macro]))

(defalias fixit fn/fix)
(alias-var 'as-macro #'macro/anon-macro)
(alias-ns 'useful.string)

(deftest test-var-name
  (is (= 'clojure.core/inc (var-name #'inc))))

(deftest test-defalias
  (is (= 1 (fixit 0 even? inc))))

(deftest test-alias-var
  (is (= 3 (as-macro [x y] `(+ ~x ~y) 1 2))))

(deftest test-alias-ns
  (is (bound? #'useful.ns-test/camelize)))