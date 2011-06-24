(ns useful.utils-test
  (:use clojure.test useful.utils))

(deftest test-or-min
  (is (= 3   (or-min nil 4 3 nil 9)))
  (is (= 1   (or-min 1 2 3 4)))
  (is (= 1   (or-min 1 nil)))
  (is (= nil (or-min nil nil nil))))

(deftest test-or-max
  (is (= 9   (or-max nil 4 3 nil 9)))
  (is (= 4   (or-max 1 2 3 4)))
  (is (= 1   (or-max 1 nil)))
  (is (= nil (or-max nil nil nil))))

(deftest test-conj-vec
  (is (= (conj-vec nil  5) [5]))
  (is (= (conj-vec '(4) 5) [4 5]))
  (is (= (conj-vec #{3} 5) [3 5])))

(deftest test-conj-set
  (is (= (conj-set nil  5) #{5}))
  (is (= (conj-set '(4) 5) #{4 5}))
  (is (= (conj-set [2]  5) #{2 5})))

(deftest test-into-vec
  (is (= (into-vec nil  [3 4]) [3 4]))
  (is (= (into-vec '(4) [5])   [4 5]))
  (is (= (into-vec [2]  [5 6]) [2 5 6])))

(deftest test-include?
  (is (include? 5 [1 2 3 4 5]))
  (is (include? :bar '(1 4 :bar)))
  (is (not (include? 2 '(1 3 4))))
  (is (not (include? :foo [1 :bar :baz 3]))))

(deftest test-split-vec
  (is (= [[1 2] [3 4]]       (split-vec [1 2 3 4]     2)))
  (is (= [[1 2] [3 4] [5 6]] (split-vec [1 2 3 4 5 6] 2 4)))
  (is (= [[1] [2 3 4 5] [6]] (split-vec [1 2 3 4 5 6] 1 5)))
  (is (= [[1] [2 3 4] [5 6]] (split-vec [1 2 3 4 5 6] 1 -2))))

(deftest test-if-ns
  (if-ns (:use this-namespace.should-not-exist)
    (is false)
    (is true))
  (if-ns (:require clojure.string)
    (is true)
    (is false)))

(deftest test-adjoin
  (is (= {:a [1 2 3] :b {"foo" [2 3 5] "bar" 7 "bap" 9 "baz" 2} :c #{2 4 6 8}}
         (adjoin
          {:a [1]    :b {"foo" [2 3] "bar" 8 "bap" 9} :c #{2 3 4 6}}
          {:a [2 3]  :b {"foo" [5]   "bar" 7 "baz" 2} :c {3 false 8 true}}))))

(deftest test-while-let
  (let [a (atom '(1 2 3 4 5))]
    (while-let [val (seq @a)]
      (is val)
      (swap! a rest))
    (is (empty? @a))))

(deftest test-queue
  (let [q (queue)]
    (is (instance? clojure.lang.PersistentQueue q))
    (is (empty? q)))
  (let [q (queue [1 2 3 4])]
    (is (= 1 (first q)))
    (is (= 2 (-> q pop first)))
    (is (= 3 (-> q pop pop first)))
    (is (= 4 (-> q pop pop pop first)))
    (is (= 4 (count q)))))

(deftest test-let-if
  (doseq [a [1 2]]
    (let-if (even? a)
            [odd  false true
             even true  false]
      (is (= even (even? a)))
      (is (= odd  (odd?  a))))))

(def *i* 1)

(deftest test-memoize-deref
  (let [count (atom 0)
        incr  (memoize-deref [#'*i*]
                (fn [i]
                  (swap! count inc)
                  (+ i *i*)))]
    (dotimes [n 5]
      (binding [*i* 4]
        (is (= 9 (incr 5)))
        (is (= 1 (incr -3))))
      (binding [*i* 1]
        (is (= 6  (incr 5)))
        (is (= -2 (incr -3)))))
    (is (= 4 @count))))
