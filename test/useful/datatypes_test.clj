(ns useful.datatypes-test
  (:use clojure.test useful.datatypes))

(defrecord Test [a b c])
(record-accessors Test)


(defprotocol Inline
  (foo [this]))
(defprotocol Dynamic
  (bar [this]))

(defrecord Implementor [x]
  Inline
  (foo [this] "x"))

(extend-type Implementor
  Dynamic
  (bar [this] "y"))

(deftest test-record
  (let [init (Test. 1 2 3)
        second (Test. 1 5 4)]
    (is (= init (make-record Test :b 2 :a 1 :c 3)))
    (is (= second (assoc-record init :b 5 :c 4)))
    (is (= second (update-record init (+ b 3) (inc c))))
    (is (= (:a init) (a init)))
    (testing "Preserves metadata"
      (let [m {:test 1}
            r (Test. 1 2 3 m {})]
        (is (= m (meta (assoc-record r :b 10))))))
    (testing "Inline typehinting"
      (is (= second (assoc-record ^Test (assoc init :b 5) :c 4))))

    (testing "Don't eval more than once"
      (let [times-evaled (atom 0)
            r (Test. 1 2 3)]
        (assoc-record ^Test (do (swap! times-evaled inc) r) :a :x :b :y :c :z)
        (is (= 1 @times-evaled))))

    (testing "Works with implemented protocols"
      (let [r (Implementor. 1)]
        (assoc-record r :x 5)))))
