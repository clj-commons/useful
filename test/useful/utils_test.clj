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

(deftest test-verify
  (is (thrown? Throwable (verify false "Test"))))

(def memo-called (atom 0))
(defm sample-memoized [x]
  (swap! memo-called inc)
  (inc x))

(deftest test-defm
  (let [i @memo-called
        j (inc i)]
    (is (= j (sample-memoized i)))
    (is (= j @memo-called))
    (is (= j (sample-memoized i)))
    (is (= j @memo-called))))

(deftest test-with-adjustments
  (is (= 1 (with-adjustments #(fnil % 0) [+ inc]
             (+ nil (inc nil))))))

(deftest test-syntax-quote
  (is (= '((quote foo) (quote (bar [baz] "hi"))) (syntax-quote '(foo (bar [baz] "hi"))))))

(deftest test-pair
  (testing "map-entry is a macro (for performance)"
    (let [form '(map-entry 1 2)]
      (is (not= form (macroexpand form)))))
  (testing "map-entry works, and is a MapEntry"
    (let [p (map-entry 1 2)
          [x y] p]
      (is (= x 1))
      (is (= y 2))
      (is (= p [1 2]))
      (are [c] (instance? c p)
           clojure.lang.IMapEntry
           clojure.lang.IPersistentVector)))
  (testing "pair is a non-macro version of map-entry"
    (is (= [(map-entry 1 2) (map-entry 3 4)]
           (map pair [1 3] [2 4])))))

(deftest test-invoke
  (is (= 1 (invoke inc 0)))
  (is (= (range 5)
         (map invoke
              (map constantly
                   (range 5))))))
