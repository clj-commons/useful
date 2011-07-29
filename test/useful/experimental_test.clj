(ns useful.experimental-test
  (:use clojure.test useful.map useful.experimental))

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

(def call-log (atom []))

(protocol-stub StubImpl
               (fn
                 ([f [this & args]]
                    (reset! call-log (keyed [f args])))
                 ([f [this & args] ret]
                    (reset! call-log (keyed [f args ret]))))
               {Sample {:default :forward}
                Define {:default :stub,
                        :exceptions [lookup]}})

(deftest stub-test
  (let [real-impl (Implementor.)
        stub-impl (StubImpl. real-impl)]
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