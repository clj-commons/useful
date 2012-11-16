(ns flatland.useful.utils-test
  (:use clojure.test flatland.useful.utils))

(deftest test-invoke
  (is (= 1 (invoke inc 0)))
  (is (= (range 5)
         (map invoke
              (map constantly
                   (range 5))))))

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

(deftest test-returning
  (let [side-effects (atom 0)]
    (is (= "TEST"
           (returning "TEST"
             (swap! side-effects inc))))
    (is (= 1 @side-effects))))

(deftest test-into-set
  (is (= #{1 2 3 4}
         (into-set #{3 1 5} {5 false 4 true 2 true}))))

(deftest test-adjoin
  (is (= {:a [1 2 3] :b {"foo" [2 3 5] "bar" 7 "bap" 9 "baz" 2} :c #{2 4 6 8}}
         (adjoin
          {:a [1]    :b {"foo" [2 3] "bar" 8 "bap" 9} :c #{2 3 4 6}}
          {:a [2 3]  :b {"foo" [5]   "bar" 7 "baz" 2} :c {3 false 8 true}}))))

(deftest test-pop-if
  (is (= [[1 2 3]   4]   (pop-if [1 2 3 4] even?)))
  (is (= [[1 2 3 4] 1]   (pop-if [1 2 3 4] odd? 1)))
  (is (= ['(2 3)    1]   (pop-if '(1 2 3) odd?)))
  (is (= ['(1 2 3)  nil] (pop-if '(1 2 3) even?)))
  (is (= ['(2)      1]   (pop-if (cons 1 [2]) odd?)))
  (is (= ['(1 2)    nil] (pop-if (cons 1 [2]) neg?))))

(deftest test-update-peek
  (is (= [1 2 4] (update-peek [1 2 3] inc)))
  (is (= [1 2 6] (update-peek [1 2 3] + 1 2)))
  (is (= '(2 2 3) (update-peek '(1 2 3) inc)))
  (is (= [{:foo 1}] (update-peek [{}] assoc :foo 1))))

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

(def ^{:dynamic true} *i* 1)

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

(deftest thread-locals
  (let [times-called (atom 0)
        inst (thread-local
              (swap! times-called inc)
              (gensym))]
    (testing "thread-local caches return values"
      (is (= 0 @times-called))
      (is (symbol? @inst))
      (is (= 1 @times-called))
      (is (symbol? @inst))
      (is (= 1 @times-called)))

    (testing "thread has only one thread-local"
      (is (= @inst @inst)))

    (testing "new thread gets new value"
      (is (not= @inst @(future @inst))))))

(deftest test-let-later
  (let-later [a (atom 0)
              b (swap! a inc)
              ^{:delay true} c (swap! a inc)]
    (is (= 1 b))
    (is (= 1 @a) "delay shouldn't have been forced yet")
    (is (= 2 c) "delay should fire when its value is needed")
    (is (= 2 @a) "and now the atom should have changed")
    (is (= 2 c) "shouldn't be eval'd again")
    (is (= 2 @a))))

(deftest test-copy-meta
  (let [x (-> [1 2 3]
              (with-meta {:foo 1}))
        y [4 5 6]
        z (copy-meta y x)]
    (is (= y z))
    (is (= (meta z) (meta x)))))

(deftest test-empty-coll
  (are [x] (empty-coll? x)
       nil, (), {}, [])
  (are [x] (not (empty-coll? x))
       "", [1], [[]], '(()),
       1, {1 2}))

(deftest test-switch
  (testing "without default"
    (is (= :a  (switch #{1}, #{1} :a, (2 3) :b, inc :c)))
    (is (= :b  (switch 2,    #{1} :a, (2 3) :b, inc :c)))
    (is (= :b  (switch 3,    #{1} :a, (2 3) :b, inc :c)))
    (is (= :c  (switch inc,  #{1} :a, (2 3) :b, inc :c)))
    (is (= nil (switch :foo, #{1} :a, (2 3) :b, inc :c))))
  (testing "with default"
    (is (= :a (switch #{1}, #{1} :a, (2 3) :b, inc :c, :d)))
    (is (= :b (switch 2,    #{1} :a, (2 3) :b, inc :c, :d)))
    (is (= :b (switch 3,    #{1} :a, (2 3) :b, inc :c, :d)))
    (is (= :c (switch inc,  #{1} :a, (2 3) :b, inc :c, :d)))
    (is (= :d (switch :foo, #{1} :a, (2 3) :b, inc :c, :d)))))
