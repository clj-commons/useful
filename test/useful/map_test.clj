(ns useful.map-test
  (:use clojure.test useful.map))

(deftest test-assoc-or
  (is (= {:a 1 :b 2 :c 3}
         (-> {:a 1 :b nil}
             (assoc-or :a 2)
             (assoc-or :b 2)
             (assoc-or :c 3)))))

(deftest test-keyed
  (let [a 1 b 2]
    (is (= {:a 1 :b 2} (keyed [a b])))
    (is (= '{a 1 b 2}  (keyed :syms [a b])))))

(deftest test-into-map
  (is (= {:foo "1", :bar "2", :bang "3", :baz "4", :blah 5}
         (into-map :foo 1 :bar 2 :bang 3 [:foo "1" :baz "4"] :bar "2" '(:bang "3") {:blah 5})))
  (is (= {:foo {:bap 3, :baz 2, :bar 1}}
         (into-map merge-in :foo {:bar 1} {:foo {:baz 2}} [:foo {:bap 3}]))))

(deftest test-map-vals
  (is (= {:foo 1 :bar 9 :baz 4}
         (map-vals {:foo 0 :bar 8 :baz 3} inc))))

(deftest test-map-keys
  (is (= {"foo" 1 "bar" 2 "baz" 3}
         (map-keys {:foo 1 :bar 2 :baz 3} name))))

(deftest test-map-vals-with-keys
  (is (= {1 3, 7 8, 9 14}
         (map-vals-with-keys {1 2, 7 1, 9 5} +))))

(deftest test-map-keys-and-vals
  (is (= {"a" "b" "c" "d"}
         (map-keys-and-vals {:a :b :c :d} name))))

(deftest test-update
  (is (= {:a 3 :b 3 :c nil}
         (-> {:a 2 :b 4 :c ()}
             (update :a inc)
             (update :b dec)
             (update :c seq)))))

(deftest test-update-each
  (is (= {:a 6 :b 8}
         (-> {:a 3 :b 4}
             (update-each [:a :b] * 2)))))

(deftest test-update-dissoc
  (is (= {:a 3 :b 3}
         (-> {:a 2 :b 4 :c ()}
             (update-dissoc :a inc)
             (update-dissoc :b dec)
             (update-dissoc :c seq)))))

(deftest test-merge-in
  (is (= {:a {:b {:c 4} :d 2 :e 3} :e 3 :f 2 :g 1}
         (merge-in {:a {:b {:c 1} :d 2} :e 3 :f 4}
                   {:a {:b {:c 4} :e 3} :f 2 :g 1})))
  (is (= {:a {:b {:c 1 :d 2} :e 2}}
         (merge-in {:a {:b {:c 1}}}
                   {:a {:b {:d 2}}}
                   {:a {:b {} :e 2}}))))

(deftest test-map-to
  (is (= {1 2 3 4 5 6} (map-to inc [1 3 5])))
  (is (= {2 1}         (map-to dec [2 2 2]))))

(deftest test-index-by
  (is (= {true 3 false 4} (index-by odd? [1 3 4])))
  (is (= {1 2 3 4 5 6}    (index-by dec  [2 4 6]))))

(deftest test-position
  (is (= (position [1 3 5 3])
         {1 0 3 1 5 2})))

(deftest map-filtering-tests
  (let [m '{a 0, b 1, c 11, d 92}]
    (is (= '#{a d} (filter-keys-by-val even? m)))
    (is (= '#{b c} (remove-keys-by-val even? m)))
    (is (= '{a 0} (filter-vals m zero?)))
    (is (= '{b 1, c 11, d 92} (remove-vals m zero?)))
    (is (= '{a 0} (filter-keys m '#{a})))
    (is (= '{b 1, c 11, d 92} (remove-keys m '#{a})))))

(deftest test-update-in
  (is (= [1] (-> (update-in! {:foo (transient {:bar []})} [:foo :bar] conj 1)
                 :foo :bar))))

(deftest test-assoc-in
  (is (= [1] (-> (assoc-in! {:foo {}} [:foo :bar] [1])
                 :foo :bar))))

(deftest test-multi-map
  (is (= {:foo #{1 2 3 4}, :bar #{2 3 4 5 6}, :baz #{5 6}}
         (multi-map {:foo 1, #{:foo :bar} #{2 3 4}, #{:baz :bar} #{5 6}})))
  (is (= {:foo #{1 2}, :bar #{2 3}}
         (multi-map {:foo #{1 2}, :bar #{2 3}}))))
