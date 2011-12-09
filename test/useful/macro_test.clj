(ns useful.macro-test
  (:use clojure.test useful.macro))

;; necessary because deftest does weird shit with namespaces, resolution, and
;; macroexpansion, so this can't be inside there
(let [strip-extraneous-do (fn [form]
                            (->> form
                                 (iterate second)
                                 (drop-while (comp #{`do} first))
                                 first))
      expansion (macroexpand '(anon-macro [name num]
                                `(inc ~(symbol (str name num)))
                                test 1))]

  (deftest test-macro-toys
    (is (= `(inc ~'test1)
           (strip-extraneous-do expansion)))
    (is (= "123abc"
           (with-out-str
             (macro-do [x] `(print '~x)
               123
               abc))))))

(def ^{:dynamic true} *value* 1)

(deftest test-alter-var
  (let [get-value (fn [] *value*)]
    (is (= 1 *value*))
    (is (= 4 (with-altered-var [*value* + 3]
               (get-value))))))
