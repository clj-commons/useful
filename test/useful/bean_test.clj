(ns useful.bean-test
  (:use clojure.test useful.bean)
  (:import [java.util Date GregorianCalendar]))

(def february (make-bean Date {:month 1}))

(deftest test-make-bean
  (is (= 1 (.getMonth february))))

(deftest test-update-bean
  (is (= 3 (.getMonth (update-bean february {:month 3})))))

(deftest test-make-bean-dashized
  (let [time (make-bean GregorianCalendar {:time-in-millis 8000000})]
    (is (= 8000000 (.getTimeInMillis time)))))
