(ns flatland.useful.fn-test
  (:use clojure.test flatland.useful.fn))

(deftest test-validator
  (is (= [0 2 4 6 8]
         (keep (validator even?)
               (range 10)))))

(deftest test-decorate
  (is (= [[1 2] [2 3] [3 4]] (map (decorate inc) [1 2 3]))))

(deftest test-annotate
  (is (= [1 2] (annotate 1 inc))))

(deftest test-fix
  (let [repair (fn [val]
                 (-> (* val 2)
                     int
                     (fix zero? dec, even? (partial * 3), inc)))]
    (is (= 12 (repair 2)))
    (is (=  4 (repair 1.5)))
    (is (= -1 (repair 0)))))

(deftest test-to-fix
  (is (= [1 -2 3 -4] (map (to-fix (! odd?) -) [1 2 3 4]))))

(deftest test-as-fn
  (is (= 3    ((as-fn 3)       :foo)))
  (is (= :foo ((as-fn #{:foo}) :foo)))
  (is (= 9    ((as-fn inc)     8))))

(deftest test-fixing
  (let [m (atom {:x 1})]
    (is (= {:x 3}
           (swap! m update-in [:x] fixing odd? + 2)))
    (is (= {:x 1}
           (fixing {:x 1} seq? conj 1 2 3 4)))))

(deftest test-given
  (is (= 1
         (-> {:value 0}
             (given map? (update-in [:value] inc)) ; matches
             (given sequential? reverse) ; doesn't match
             (given :value :value))))
  (is (= {:value 1}
         (-> {:value 0}
             (given map? (update-in [:value] inc) ; matches
                    sequential? reverse ; these next two are never tested
                    :value :value))))
  (is (= 4
         (-> 3
             (given map? (update-in [:value] inc) ; matches
                    sequential? reverse ; these next two are never tested
                    inc)))))

(deftest test-any
  (is (= [0 2 3 4 6 8 9 10]
         (filter (any #(zero? (rem % 2))
                      #(zero? (rem % 3)))
                 (range 11)))))

(deftest test-all
  (is (= [0 6]
         (filter (all #(zero? (rem % 2))
                      #(zero? (rem % 3)))
                 (range 11)))))

(deftest test-knit
  (is (= [5 \t 9]
         ((knit inc last #(* 3 %))
          [4 "last" 3])))
  (is (= {"A" 10 "B" 1}
         (into {}
               (map (knit #(.toUpperCase %) inc)
                    {"a" 9 "b" 0})))))

(deftest test-thrush
  (is (= 5 (thrush 1 inc inc inc inc))))

(deftest test-ignoring-nils
  (is (= 6 ((ignoring-nils +) 1 nil 2 nil nil 3)))
  (is (= 0 ((ignoring-nils +) nil nil)))
  (is (= 0 ((ignoring-nils +) nil nil nil)))
  (is (= 0 ((ignoring-nils +) nil nil nil nil))))

(deftest test-key-comparator
  (let [subtract-comparator-fn-breaks-on-this [2147483650 2147483651
                                               2147483652 4 2 3 1]
        normal-cmp (key-comparator identity)]
    (is (= (sort subtract-comparator-fn-breaks-on-this)
           (sort normal-cmp subtract-comparator-fn-breaks-on-this))))
  (let [square (fn [x] (* x x))
        by-square (key-comparator :ascending square)]
    (is (= (sort-by square [-9 -5 1 -2])
           (sort by-square [-9 -5 1 -2])))))

(deftest test-=to
  (let [objs [1 :x "x" [5] nil (Object.) {:x 1} '((a b c) d)]]
    (doseq [x objs
            y objs]
      (is (= (= x y) ((=to x) y))))))
