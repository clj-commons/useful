(ns useful.experimental-test
  (:use clojure.test useful.experimental
        [clojure.walk :only [stringify-keys]]))

(deftest test-while-let
  (let [a (atom '(1 2 3 4 5))]
    (while-let [val (seq @a)]
      (is val)
      (swap! a rest))
    (is (empty? @a))))

(deftest test-let-if
  (doseq [a [1 2]]
    (let-if (even? a)
            [odd  false true
             even true  false]
      (is (= even (even? a)))
      (is (= odd  (odd?  a))))))

(deftest test-dispatcher-fn
  (let [dispatch (dispatcher (fn [f & args] (symbol "clojure.core" f)))]
    (is (= "str5" (dispatch "str" 5)))))

(deftest test-dispatch
  (testing "simple dispatch"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.set" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse")))
    (is (= {2 :b, 1 :a} (invert {:a 1 :b 2})))
    (is (= [:bar :foo] (invert [:foo :bar]))))

  (testing "flat hierarchy"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.core" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse"))
      :type-hierarchy '{clojure.core clojure.set})
    (is (= {2 :b, 1 :a} (invert {:a 1 :b 2})))
    (is (= [:bar :foo] (invert [:foo :bar]))))

  (testing "deep hierarchy"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.core" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse"))
      :type-hierarchy '{clojure.core clojure.foo
                        clojure.foo  clojure.bar
                        clojure.bar  clojure.set})
    (is (= {2 :b, 1 :a} (invert {:a 1 :b 2})))
    (is (= [:bar :foo] (invert [:foo :bar]))))

  (testing "dispatch to ns does not exist"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.foo" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse")))
    (is (thrown? java.lang.IllegalArgumentException
                 (invert {:a 1 :b 2}))))

  (testing "dispatch to fn does not exist"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.set" "foo")

                               (vector? %)
                               (symbol "clojure.core" "reverse")))
    (is (thrown? java.lang.IllegalArgumentException
                 (invert {:a 1 :b 2}))))

  (testing "middleware"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.set" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse"))
      :wrapper #(fn [arg]
                  (if (map? arg)
                    (% (stringify-keys arg))
                    (% arg))))
    (is (= {2 "b" 1 "a"} (invert {:a 1 :b 2})))
    (is (= [:bar :foo] (invert [:foo :bar])))))