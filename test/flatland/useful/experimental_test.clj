(ns flatland.useful.experimental-test
  (:use clojure.test flatland.useful.map
        flatland.useful.experimental
        flatland.useful.experimental.delegate)
  (:require [flatland.useful.utils :as utils]))

(deftest test-while-let
  (let [a (atom '(1 2 3 4 5))]
    (while-let [val (seq @a)]
      (is val)
      (swap! a rest))
    (is (empty? @a))))

(deftest test-let-if
  (doseq [a [1 2]]
    (let-if (even? a)
            [odd  false true
             even true  false]
      (is (= even (even? a)))
      (is (= odd  (odd?  a))))))

(deftest test-order-let-if
  (order-let-if true
                [a 1, b 2]
    (is (and (= a 1) (= b 2))))
  (order-let-if false
                [a 1, b 2]
    (is (and (= a 2) (= b 1)))))


;;; protocols defined for testing protocol-stub
(defprotocol Sample
  (sample [this data]))

(defprotocol Define
  (define [this k v])
  (lookup [this k]))

(defrecord Implementor []
  Sample
  (sample [this data] 10)

  Define
  (define [this k v] false)
  (lookup [this k] :not-found))

(protocol-stub StubImpl
               {Sample {:default :forward}
                Define {:default :stub,
                        :exceptions [lookup]}})

(deftest stub-test
  (let [call-log (atom [])
        real-impl (Implementor.)
        stub-impl (StubImpl. real-impl
                             (fn
                               ([f [this & args]]
                                  (reset! call-log (keyed [f args])))
                               ([f [this & args] ret]
                                  (reset! call-log (keyed [f args ret])))))]
    (testing "default action works without exceptions"
      (is (= [] @call-log))
      (is (= 10 (sample real-impl 'whatever)))
      (is (= [] @call-log))
      (is (= 10 (sample stub-impl 'whatever)))
      (is (= {:f 'sample, :args ['whatever], :ret 10} @call-log)))

    (testing "default action works with a different exception"
      (is (false? (define real-impl 1 2)))
      (is (nil? (define stub-impl 1 2)))
      (is (= {:f 'define :args [1 2]} @call-log)))

    (testing "exceptions are applied"
      (is (= :not-found (lookup real-impl 1)))
      (is (= :not-found (lookup stub-impl 1)))
      (is (= {:f 'lookup :args [1] :ret :not-found} @call-log)))))

(deftest wrapper-test
  (with-local-vars [dummy-wrapper ()]
    (testing "Wrapping respects manually-established bindings"
      (with-local-vars [wrappers ()]
        (defn-wrapping my-inc wrappers "add one" [x]
          (+ 1 x))
        (is (= 2 (my-inc 1)))
        (let [start-num 1]
          (is (= (* 2 (inc (+ 10 start-num)))
                 (with-bindings {wrappers (list (fn [f] ;; outermost wrapper
                                                  (fn [x]
                                                    (* 2 (f x))))
                                                (fn [f] ;; innermost wrapper
                                                  (fn [x]
                                                    (f (+ 10 x)))))}
                   (my-inc start-num)))))
        (let [call-log (atom nil)]
          (is (= 2 (with-bindings {wrappers (list (fn [f]
                                                    (fn [x]
                                                      (let [ret (f x)]
                                                        (reset! call-log [(-> wrappers deref meta :flatland.useful.experimental/call-data :fn-name) x ret])
                                                        ret))))}
                     (my-inc 1))))
          (testing "Wrapping-related metadata bound correctly"
            (is (= ['my-inc 1 2] @call-log))))))

    (testing "with-wrapper(s) works"
      (let [prepend (fn [item] (fn [f] (fn [& args] (apply f item args))))
            append (fn [item] (fn [f] (fn [& args] (apply f (concat args [item])))))]
        (with-local-vars [vec-wrapper []
                          cons-wrapper ()]
          (defn-wrapping vec-str vec-wrapper "Make stuff a string" [& args]
            (apply str args))
          (defn-wrapping cons-str cons-wrapper "Make stuff a string" [& args]
            (apply str args))
          (with-wrapper vec-wrapper (prepend 'foo)
            (is (= "foo123" (vec-str 1 2 3)))
            (with-wrapper vec-wrapper (append 'bar)
              (is (= "foo123bar" (vec-str 1 2 3)))
              (with-wrapper vec-wrapper (prepend 'baz)
                (is (= "foobaz123bar" (vec-str 1 2 3))))))
          (with-wrappers cons-wrapper [(prepend 'foo) (append 'bar) (prepend 'baz)]
            (is (= "bazfoo123bar" (cons-str 1 2 3)))))))

    (testing "Metadata is applied properly"
      (defn-wrapping myfn dummy-wrapper "re-implement clojure.core/first." [[x]]
        x)
      (let [meta (meta #'myfn)]
        (is (= '([[x]]) (:arglists meta)))
        (is (= "re-implement clojure.core/first." (:doc meta))))

      (testing "Docstring is optional"
        (defn-wrapping testfn dummy-wrapper [x]
          (inc x))
        (is (= 1 (testfn 0)))))

    (let [inc-fn (fn [f] (comp inc f))]
      (testing "Wrapper can be added after function is defined"
        (defn frizzle [x] (inc x))
        (make-wrappable! #'frizzle dummy-wrapper)
        (is (= 3 (with-wrapper dummy-wrapper inc-fn
                   (frizzle 1)))))

      (testing "wrap-multiple"
        (defn frazzle [x] (inc x))
        (defn zazzle [x] (inc x))
        (wrap-multiple dummy-wrapper frazzle zazzle)
        (are [f] (= 3 (with-wrapper dummy-wrapper inc-fn
                        (f 1)))
             frazzle zazzle)))))

(deftest fixes-test
  (is (= 4 (fixes {:value 9}
                  map? :value
                  string? read-string
                  odd? dec
                  even? #(/ % 2))))
  (let [a (atom 0)]
    (is (thrown? Exception
                 (fixes a
                        identity #(swap! % inc)
                        identity)))
    (is (= 0 @a) "Should throw an exception before trying any clauses")))

(deftest lift-meta-test
  (let [m (lift-meta {:a 1 :b 2} :a)]
    (is (= {:b 2} m))
    (is (= {:a 1} (meta m)))))

(deftest prefix-lookup-test
  (let [lookup (prefix-lookup [["a" :apple]
                               ["person" :person]
                               [:p :pineapple]
                               ["abbrev" :abbreviation]])]
    (are [in out] (= out (lookup in))
         "apropos" :apple
         "persona" :person
         "pursues" :pineapple ;; keywords should work
         "abbrev." :apple ;; should test in order, and short-circuit
         )))

(deftest canonical-name-test
  (is (= 'clojure.core/inc (canonical-name 'inc)))
  (is (= 'java.lang.Object (canonical-name 'Object)))
  (is (= 'flatland.useful.utils/adjoin (canonical-name `utils/adjoin))))
