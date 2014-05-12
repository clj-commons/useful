(ns flatland.useful.compress-test
  (:use clojure.test flatland.useful.compress))


(deftest round-trip
  (let [s "f3509ruwqerfwoa reo1u30`1 ewf dfgjdsf sfc saf65sad+ f5df3
g2 sd35g4szdf sdf4 as89faw76fwfwf210
"]
    (is (= s (unsmash (smash s))))))
