(ns test-useful
  (:use clojure.test useful))

(deftest test-args-map
  (is (= {:foo "1", :bar "2", :bang "3", :baz "4", :blah 5} 
         (args-map :foo 1 :bar 2 :bang 3 [:foo "1" :baz "4"] :bar "2" '(:bang "3") {:blah 5}))))
