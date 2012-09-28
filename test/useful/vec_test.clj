(ns useful.vec-test
  (:use clojure.test useful.vec))

(deftest test-avec
  (let [avec [[1 :a] [:foo [1 2 3]] ["bar" 42] [:none nil]]]
    (testing "avec-index"
      (is (= 0   (avec-index avec 1)))
      (is (= 2   (avec-index avec "bar")))
      (is (= 1   (avec-index avec :foo)))
      (is (= nil (avec-index avec :baz))))
    (testing "avec-find"
      (is (= [1 :a]         (avec-find avec 1)))
      (is (= [:foo [1 2 3]] (avec-find avec :foo)))
      (is (= ["bar" 42]     (avec-find avec "bar")))
      (is (= [:none nil]    (avec-find avec :none)))
      (is (= nil            (avec-find avec 3))))
    (testing "avec-get"
      (is (= :a      (avec-get avec 1)))
      (is (= [1 2 3] (avec-get avec :foo)))
      (is (= 42      (avec-get avec "bar")))
      (is (= nil     (avec-get avec 3)))
      (is (= :nope   (avec-get avec 3 :nope)))
      (is (= :a      (avec-get avec 1 :yep)))
      (is (= nil     (avec-get avec :none 42))))
    (testing "avec-keys"
      (is (= [1 :foo "bar" :none] (avec-keys avec))))
    (testing "avec-vals"
      (is (= [:a [1 2 3] 42 nil] (avec-vals avec))))
    (testing "avec-assoc"
      (let [bvec (avec-assoc avec 1 :one :b 4 :c 8)]
        (is (= :one (avec-get bvec 1)))
        (is (= 4    (avec-get bvec :b)))
        (is (= 8    (avec-get bvec :c)))
        (is (= [1 :foo "bar" :none :b :c] (avec-keys bvec)))
        (let [cvec (avec-assoc bvec 1 nil :b 5)]
          (is (= nil (avec-get cvec 1)))
          (is (= 5   (avec-get cvec :b)))
          (is (= [1 :foo "bar" :none :b :c] (avec-keys cvec))))))
    (testing "avec-dissoc"
      (let [dvec (avec-dissoc avec 1 :foo :bar)]
        (is (= nil        (avec-find dvec 1)))
        (is (= nil        (avec-find dvec :foo)))
        (is (= ["bar" 42] (avec-find dvec "bar")))
        (is (= ["bar" :none] (avec-keys dvec)))
        (is (= [42 nil]      (avec-vals dvec)))))
    (testing "avec-update"
      (let [evec (-> avec
                     (avec-update :foo conj 4)
                     (avec-update "bar" inc))]
        (is (= :a        (avec-get evec 1))) ; unchanged
        (is (= [1 2 3 4] (avec-get evec :foo)))
        (is (= 43        (avec-get evec "bar")))
        (is (= [1 :foo "bar" :none] (avec-keys evec)))
        (let [fvec (avec-update evec :new assoc :foo 1 :bar 2)]
          (is (= {:foo 1 :bar 2} (avec-get fvec :new)))
          (is (= :a              (avec-get fvec 1))) ; unchanged
          (is (= [1 :foo "bar" :none :new] (avec-keys fvec))))))))
