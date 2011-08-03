(ns useful.java-test
  (:use clojure.test useful.java))

(deftest test-rescue
  (is (= nil (rescue (/ 9 0) nil)))
  (is (= 3   (rescue (/ 9 3) nil))))

(deftest test-construct
  (is (= "test" (construct String "test"))))

(deftest test-invoke-private
  (let [hash (doto (java.util.Hashtable.)
               (.put 1 2)
               (.put 3 4))]
    (is (thrown? Throwable (.rehash hash)))
    (is (= {1 2 3 4}
           (doto hash (invoke-private "rehash"))))
    (is (thrown? Throwable (.rehash hash)))))
