(ns flatland.useful.ns-test
  (:use clojure.test flatland.useful.ns)
  (:require [flatland.useful.fn :as fn]
            [flatland.useful.macro :as macro]))

(defalias fixit fn/fix)
(alias-var 'as-macro #'macro/anon-macro)
(alias-ns 'flatland.useful.string)

(deftest test-var-name
  (is (= 'clojure.core/inc (var-name #'inc))))

(deftest test-defalias
  (is (= 1 (fixit 0 even? inc))))

(deftest test-alias-var
  (is (= 3 (as-macro [x y] `(+ ~x ~y) 1 2))))

(deftest test-alias-ns
  (is (bound? #'flatland.useful.ns-test/camelize)))