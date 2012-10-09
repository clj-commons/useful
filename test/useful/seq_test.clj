(ns useful.seq-test
  (:use clojure.test useful.seq clojure.set))

(deftest test-zip
  (is (= [[1 4 8] [2 5 9] [3 6 nil] [nil 7 nil]] (zip [1 2 3] [4 5 6 7] [8 9]))))

(deftest test-insert
  (is (= [1 2 3 4 5] (insert [2 3] 1 [1 4 5]))))

(deftest test-find-with
  (is (= :foo (find-with odd?  [2 4 5 7] [:bar :baz :foo :bap])))
  (is (= nil  (find-with even? [1 3 5 9] [:bar :baz :foo :bap]))))

(deftest test-cross
  (is (= '((0 0) (0 1) (1 0) (1 1))          (cross [0 1] [0 1])))
  (is (= '((0 0 2) (0 1 2) (1 0 2) (1 1 2))) (cross [0 1] [0 1] [2])))

(deftest test-lazy-cross
  (is (= '((0 0) (1 0) (0 1) (1 1))         (lazy-cross [0 1] [0 1])))
  (is (= '((0 0 2) (1 0 2) (0 1 2) (1 1 2)) (lazy-cross [0 1] [0 1] [2]))))

(deftest test-extract
  (is (= [5 '(2 4 6 1 2 7)] (extract odd?     [2 4 6 5 1 2 7])))
  (is (= [2 '(4 6 5 1 2 7)] (extract even?    [2 4 6 5 1 2 7])))
  (is (= [7 '(2 4 6 5 1 2)] (extract #(< 6 %) [2 4 6 5 1 2 7]))))

(deftest test-separate
  (is (= ['(5 1 7) '(2 4 6 2)] (separate odd?  [2 4 6 5 1 2 7])))
  (is (= ['(2 4 6 2) '(5 1 7)] (separate even? [2 4 6 5 1 2 7]))))

(deftest test-glue
  ;; Make sure all items of the same type wind up in the same batch,
  ;; and each batch is as close to size 6 as possible without going over.

  ;; The D batch is too large, and glue promises to return a too-large batch
  ;; in preference to splitting up a batch.
  (is (= '((a1 a2 a3 a4 b1)
           (c1 c2)
           (d1 d2 d3 d4 d5 d6 d7)
           (e8))
         (glue into []
               (fn [batch more]
                 (>= 6 (+ (count batch) (count more))))
               (constantly false)
               '((a1 a2 a3 a4)
                 (b1)
                 (c1 c2)
                 (d1 d2 d3 d4 d5 d6 d7)
                 (e8))))))

(deftest test-partition-between
  (testing "returns a totally lazy sequence"
    (is (= (lazy-seq nil)
           (partition-between (fn [& _] (throw (Exception. "Never call me")))
                              nil))))
  (testing "doesn't force input sequence more than necessary"
    ;; partition-between should be forcing elements 1 and 2 of this sequence
    ;; to compute the first partition.
    (let [input (list* 1 2 (lazy-seq (throw (Exception. "broken"))))
          partitioned (partition-between (constantly true) input)]
      (is (= [1] (first partitioned)))
      (is (thrown? Exception (second partitioned)))))
  (let [input [1 nil nil 2 3 nil 4]]
    (are [f output] (= output (partition-between f input))
         (fn [[a b]] (not (nil? a)))           [[1] [nil nil 2] [3] [nil 4]],
         (fn [[a b]] (not (nil? b)))           [[1 nil nil] [2] [3 nil] [4]],
         (partial some nil?)                   [[1] [nil] [nil] [2 3] [nil] [4]],
         (fn [[a b]] (not= (nil? a) (nil? b))) [[1] [nil nil] [2 3] [nil] [4]])))

(deftest test-include?
  (is (include? 5 [1 2 3 4 5]))
  (is (include? :bar '(1 4 :bar)))
  (is (not (include? 2 '(1 3 4))))
  (is (not (include? :foo [1 :bar :baz 3]))))

(deftest test-unfold
  (is (= [0 1 1 2 3 5 8 13 21 34]
         (take 10 (unfold (fn [[a b]]
                            [a [b (+ a b)]])
                          [0 1])))))

(deftest test-take-shuffled
  (let [nums (set (range 10))]
    (is (= nums (set (take-shuffled (count nums) nums))))
    (is (= 5 (count (take-shuffled 5 nums))))
    (is (subset? (set (take-shuffled 3 nums)) nums))))

(deftest test-find-first
  (is (= 5 (find-first odd? [2 5 9])))
  (is (nil? (find-first (constantly false) (range 1000)))))

(deftest test-lazy-loop
  (is (= (range 10)
         (lazy-loop [i 0]
           (when-not (= i 10)
             (cons i (lazy-recur (inc i)))))))
  (testing "0-arg lazy-loop"
    (is (= [1 1 1] (take 3
                         (lazy-loop []
                           (cons 1 (lazy-recur))))))))

(deftest test-alternates
  (is (= '[[a b] [1 2]]
         (alternates '[a 1 b 2])))
  (is (= '[[0 3 6] [1 4 7] [2 5 8]]
         (alternates 3 (range 9))))
  (testing "Doesn't blow up for empty seqs"
    (let [a (alternates [])]
      (testing "Lazy if nothing forced."
        (is a))
      (is (not (seq a))))))

(deftest test-slice
  (let [size 900, slices 7, coll (range size),
        sliced (slice slices coll), largest (apply max (map count sliced))]
    (testing "We get all the items back in order"
      (is (= coll (apply concat sliced))))
    (testing "We get the right number of slices"
      (is (= slices (count sliced))))
    (testing "Slices are sized regularly"
      (is (every? #(<= (Math/abs (- % largest)) 1)
                  (map count sliced))))))

(deftest test-foldr
  (is (= [1 2 3 4]
         (foldr cons nil [1 2 3 4]))))

(deftest test-unchunk
  (let [a (atom 0)
        f (fn [_] (swap! a inc))
        coll (range 100)]
    (is (= 1 (first (map f coll))))
    (is (< 1 @a)) ;; multiple elements realized

    (reset! a 0)
    (is (= 1 (first (map f (unchunk coll)))))
    (is (= 1 @a)))) ;; only one element realized

(deftest test-lazy
  (let [realized (atom 0)
        realize (fn [x] (swap! realized inc) x)
        the-list (lazy (realize 1) (realize 2))]
    (is (= 0 @realized))
    (is (= 1 (first the-list)))
    (is (= 1 @realized))
    (is (= 2 (second the-list)))
    (is (= 2 @realized))
    (is (nil? (next (next the-list))))
    (is (= 2 @realized))))

(deftest test-prefix-of?
  (let [a [1 2 3], b [1 2], c [2 3], d []]
    (is (prefix-of? a b))
    (is (prefix-of? a a))
    (is (prefix-of? b a))
    (is (not (prefix-of? a c)))
    (is (prefix-of? a d))
    (is (prefix-of? b d))))

(deftest test-sequeue
  (testing "lookahead"
    (let [a (atom 0)
          xs (list* 1 2 3 4 5 6 7 8 9 [10]) ;; avoid chunking
          coll (for [x xs] (do (swap! a inc) x))]
      (is (zero? @a))
      (let [s (sequeue 5 coll)]
        (Thread/sleep 100)
        (is (< 0 @a 10)) ;; should have some queued, but not all
        (is (= coll (doall s)))
        (is (= 10 @a)))))
  (testing "error propagation"
    (let [coll (lazy-seq
                 (list* 1 2 3 4 5 6 7 8 9
                        (lazy-seq
                          (cons 10
                                (lazy-seq
                                  (throw (IllegalStateException. "Broken")))))))
          s (sequeue 2 coll)]
      (is (= 1 (first s)))
      (is (thrown? Throwable (dorun s))))))

(deftest test-map-nth
  (is (= [2 2 4 4 6 6 8 8 10 10]
         (map-nth inc 2 [1 2 3 4 5 6 7 8 9 10])))
  (is (= ["" "x" "" "x"] (map-nth #(str % "x") 1 2 ["" "" "" ""]))))

(deftest test-update-first
  (is (= [1 3 3 4 5]
         (update-first [1 2 3 4 5] even? inc)))

  (is (= [1 2 3 4 5 15]
         (update-first [1 2 3 4 5] zero? (fnil + 0) 1 2 3 4 5))))