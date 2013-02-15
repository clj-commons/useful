(ns flatland.useful.dispatch-test
  (:use clojure.test flatland.useful.dispatch
        [clojure.walk :only [stringify-keys]])

  ;; not used directly, but added to verify that imported functions aren't exposed via dispatcher
  (:require [clojure.set :refer [rename-keys]]))

(deftest test-dispatcher-fn
  (let [dispatch (dispatcher (fn [f & args] (symbol "clojure.core" f)))]
    (is (= "str5" (dispatch "str" 5)))))

(deftest test-imported-functions
  (let [fn-name 'flatland.useful.dispatch-test/rename-keys
        dispatch (dispatcher (constantly fn-name))]
    (prn (dispatch {:a 1} {:a :b}))
    (is (thrown? Exception (dispatch {:a 1} {:a :b})))))

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
      :hierarchy '{clojure.core clojure.set})
    (is (= {2 :b, 1 :a} (invert {:a 1 :b 2})))
    (is (= [:bar :foo] (invert [:foo :bar]))))

  (testing "deep hierarchy"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.core" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse"))
      :hierarchy '{clojure.core clojure.foo
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
      :wrap #(fn [arg]
               (if (map? arg)
                 (% (stringify-keys arg))
                 (% arg))))
    (is (= {2 "b" 1 "a"} (invert {:a 1 :b 2})))
    (is (= [:bar :foo] (invert [:foo :bar]))))

  (testing "self as sub-type"
    (defdispatch invert #(cond (map? %)
                               (symbol "clojure.core" "map-invert")

                               (vector? %)
                               (symbol "clojure.core" "reverse"))
      :hierarchy '{clojure.core clojure.foo
                   clojure.foo  clojure.foo})
    (is (thrown? java.lang.Exception
                 (invert {:a 1 :b 2})))))