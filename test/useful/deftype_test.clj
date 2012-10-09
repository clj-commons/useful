(ns useful.deftype-test
  (:use clojure.test useful.deftype))

(defn is-valid-map [inst]
  (let [m (into (empty inst)
                [[1 :a] [:foo [1 2 3]] ["bar" 42] [:none nil]])]
    (testing "find"
      (is (= [1 :a]         (find m 1)))
      (is (= [:foo [1 2 3]] (find m :foo)))
      (is (= ["bar" 42]     (find m "bar")))
      (is (= [:none nil]    (find m :none)))
      (is (= nil            (find m 3))))
    (testing "get"
      (is (= :a      (get m 1)))
      (is (= [1 2 3] (get m :foo)))
      (is (= 42      (get m "bar")))
      (is (= nil     (get m 3)))
      (is (= :nope   (get m 3 :nope)))
      (is (= :a      (get m 1 :yep)))
      (is (= nil     (get m :none 42))))
    (testing "keys"
      (is (= #{1 :foo "bar" :none} (set (keys m)))))
    (testing "vals"
      (is (= #{:a [1 2 3] 42 nil} (set (vals m)))))
    (testing "assoc"
      (let [m2 (assoc m 1 :one :b 4 :c 8)]
        (is (= :one (get m2 1)))
        (is (= 4    (get m2 :b)))
        (is (= 8    (get m2 :c)))
        (is (= #{1 :foo "bar" :none :b :c} (set (keys m2))))
        (let [m3 (assoc m2 1 nil :b 5)]
          (is (= nil (get m3 1)))
          (is (= 5   (get m3 :b)))
          (is (= #{1 :foo "bar" :none :b :c} (set (keys m3)))))))
    (testing "dissoc"
      (let [m2 (dissoc m 1 :foo :bar)]
        (is (= nil        (find m2 1)))
        (is (= nil        (find m2 :foo)))
        (is (= ["bar" 42] (find m2 "bar")))
        (is (= #{"bar" :none} (set (keys m2))))
        (is (= #{42 nil}      (set (vals m2))))))))

(deftest test-alist
  (is-valid-map (alist)))
