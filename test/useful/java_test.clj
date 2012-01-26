(ns useful.java-test
  (:use clojure.test useful.java)
  (:import (java.util Collection Map)))

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

(deftest test-hinted-let
  (let [item {:foo 10}]
    (is (= 1 (multi-hinted-let [x item [Collection Map]] (.size x)))
        "Should work when actual class matches.")
    (is (thrown? Throwable (multi-hinted-let [x item [Collection]] (.size x))
                 "Should fail when no class matches."))
    ;; TODO find a way to assert no reflection happens?
    ))
