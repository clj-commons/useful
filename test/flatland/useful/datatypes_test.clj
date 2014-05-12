(ns flatland.useful.datatypes-test
  (:use clojure.test flatland.useful.datatypes))

(deftest test-as-int
  (are [in out] (= (as-int in) out)
       "1" 1
       2 2
       4.5 4
       nil nil))

(defrecord Test [a b c])
(defrecord Other [dash-thing question? bang!])
(record-accessors Test Other)

(deftest test-munged-names
  (let [x (Other. 1 2 3)]
    (testing "Accessor functions"
      (is (= 1 (dash-thing x)))
      (is (= 2 (question? x)))
      (is (= 3 (bang! x))))

    (testing "assoc-record"
      (is (= x (assoc-record x :dash-thing 1)))
      (is (= x (assoc-record x :question? 2)))
      (is (= x (assoc-record x :bang! 3))))

    (testing "update-record"
      (is (= x (update-record x
                              (identity dash-thing)
                              (identity question?)
                              (identity bang!)))))))

(defprotocol Inline
  (foo [this]))
(defprotocol Dynamic
  (bar [this]))

(defrecord Implementor [x]
  Inline
  (foo [this] (bar this)))

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

    (testing "Works calling implemented protocols"
      (let [r (Implementor. 1)]
        (assoc-record r :x 5)))))
