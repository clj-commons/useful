(ns flatland.useful.compress-test
  (:use clojure.test
        flatland.useful.compress)
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(deftest round-trip
  (let [s "f3509ruwqerfwoa reo1u30`1 ewf dfgjdsf sfc saf65sad+ f5df3
g2 sd35g4szdf sdf4 as89faw76fwfwf210
"]
    (is (= s (unsmash (smash s))))))

(deftest smash-test
  (is (= "eJwDAAAAAAE=\n" (smash-new "")))
  (is (= "eJxLBAAAYgBi\n" (smash-new "a"))))

(deftest unsmash-test
  (is (= "" (unsmash-new "eJwDAAAAAAE=\n")))
  (is (= "a" (unsmash-new "eJxLBAAAYgBi\n"))))

(defspec round-trip-gen
  100
  (prop/for-all [s gen/string]
    (= s (unsmash-new (smash s)))))
