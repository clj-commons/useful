(ns useful.state-test
  (:use clojure.test useful.state))

(deftest test-volatile
  (testing "volatile returns a mutable ref"
    (let [a (volatile 1)]
      (is (= 1 @a))
      (is (= 2 (put! a 2)))
      (is (= 2 @a)))
    (let [a (volatile 1 :meta {:foo 1} :validator pos?)]
      (is (= 1 (:foo (meta a))))
      (is (= 1 @a))
      (is (= 2 (put! a 2)))
      (is (= 1 (:foo (meta a))))
      (is (= 2 @a))
      (is (thrown-with-msg? java.lang.IllegalStateException #"Invalid reference state"
            (put! a 0))))))

(deftest test-trade
  (testing "trade! returns the old atom value"
    (let [a (atom 1)]
      (is (= 1 (trade! a inc)))
      (is (= 2 @a))
      (is (= 2 (trade! a + 100)))
      (is (= 102 @a)))))