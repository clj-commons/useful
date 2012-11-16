(ns flatland.useful.parallel-test
  (:use clojure.test flatland.useful.parallel))

(def ^{:dynamic true} *i* 1)

(defn mult [num]
  (* num *i*))

(defn wrap-i [f]
  (fn []
    (binding [*i* 2]
      (f))))

(deftest test-pcollect
  (doseq [n [1 2 3 4]]
    (binding [*pcollect-thread-num* n]
      (is (= [1 2 3 4 5 6 7 8 9 10]
               (pcollect inc [0 1 2 3 4 5 6 7 8 9])))
      (is (= [2 4 6 8 10 12 14 16 18 20]
               (pcollect wrap-i mult
                         [1 2 3 4 5 6 7 8 9 10]))))))
