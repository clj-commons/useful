(ns flatland.useful.bean-test
  (:use clojure.test flatland.useful.bean)
  (:import (java.beans PropertyDescriptor)))

(defmethod coerce [Boolean/TYPE nil]    [_ _ val] false)
(defmethod coerce [Boolean/TYPE Object] [_ _ val] (boolean val))

(deftest beans
  (let [b (PropertyDescriptor. "bound" PropertyDescriptor)]
    (is (= false (.isBound b)))
    (is (= false (.isConstrained b)))
    (update-bean b {:bound true :constrained true})
    (is (= true (.isBound b)))
    (is (= true (.isConstrained b)))
    (testing "coercion"
      (update-bean b {:bound nil :constrained nil})
      (is (= false (.isBound b)))
      (is (= false (.isConstrained b))))))