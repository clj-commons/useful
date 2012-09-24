(ns useful.config-test
  (:use clojure.test useful.config))

(deftest reading
  (is (= '{size 1} (read-config "config1.clj"))))

(deftest loading
  (is (= {:x [1 1]
          :y [1 1]}
         (load-config "config2.clj"))))