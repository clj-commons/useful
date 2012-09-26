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

(deftest test-wait-until
  (let [a (atom 0)]
    (is (zero? (wait-until a even?)))
    (let [f (future (Thread/sleep 250)
                    (swap! a inc))]
      (is (odd? (wait-until a odd?))))))

(deftest test-with-timing
  (let [[ret ms] (with-timing
                   (+ 2 2)
                   (+ 3 3))]
    (is (= ret 6))
    (is (float? ms))))



(def ^{:dynamic true} *value* 1)

(deftest test-alter-var
  (let [get-value (fn [] *value*)]
    (is (= 1 *value*))
    (is (= 4 (with-altered-vars [(+ *value* 3)]
               (get-value))))))

(def const 20)
(deftest test-alter-root
  (let [get-value (fn [] const)]
    (is (= 20 (get-value)))
    (is (= 10 (with-altered-roots [(- const 10)]
                (get-value))))
    (is (= 20 (get-value)))))
