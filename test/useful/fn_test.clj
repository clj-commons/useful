(ns useful.fn-test
  (:use clojure.test useful.fn))

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

(deftest test-given
  (is (= 1
         (-> {:value 0}
             (given map? update-in [:value] inc) ; matches
             (given sequential? reverse) ; doesn't match
             (given :value :value)))))

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
          [4 "last" 3]))))

(deftest test-thrush
  (is (= 5 (thrush 1 inc inc inc inc))))

(deftest test-ignoring-nils
  (is (= 6 ((ignoring-nils +) 1 nil 2 nil nil 3))))
