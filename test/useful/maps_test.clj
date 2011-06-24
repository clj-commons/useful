(ns useful.maps-test
  (:use clojure.test useful.maps))

(deftest test-assoc-if
  (is (= {:a 1 :b 3}
         (-> {:a 1}
             (assoc-if (even? 3) :a 3)
             (assoc-if (odd?  3) :b 3)))))

(deftest test-assoc-or
  (is (= {:a 1 :b 2 :c 3}
         (-> {:a 1 :b nil}
             (assoc-or :a 2)
             (assoc-or :b 2)
             (assoc-or :c 3)))))

(deftest test-into-map
  (is (= {:foo "1", :bar "2", :bang "3", :baz "4", :blah 5}
         (into-map :foo 1 :bar 2 :bang 3 [:foo "1" :baz "4"] :bar "2" '(:bang "3") {:blah 5}))))

(deftest test-map-vals
  (is (= {:foo 1 :bar 9 :baz 4}
         (map-vals inc {:foo 0 :bar 8 :baz 3}))))

(deftest test-map-vals-with-keys
  (is (= {1 3, 7 8, 9 14}
         (map-vals-with-keys + {1 2, 7 1, 9 5}))))

(deftest test-update
  (is (= {:a 3 :b 3}
         (-> {:a 2 :b 4}
             (update :a inc)
             (update :b dec))))
  (is (= {:a 6 :b 8}
         (-> {:a 3 :b 4}
             (update [:a :b] * 2)))))

(deftest test-merge-in
  (is (= {:a {:b {:c 4} :d 2 :e 3} :e 3 :f 2 :g 1}
         (merge-in {:a {:b {:c 1} :d 2} :e 3 :f 4}
                   {:a {:b {:c 4} :e 3} :f 2 :g 1}))))

(deftest test-map-to
  (is (= {1 2 3 4 5 6} (map-to inc [1 3 5])))
  (is (= {2 1}         (map-to dec [2 2 2]))))

(deftest test-index-by
  (is (= {true 3 false 4} (index-by odd? [1 3 4])))
  (is (= {1 2 3 4 5 6}    (index-by dec  [2 4 6]))))
