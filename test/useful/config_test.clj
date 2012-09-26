(ns useful.config-test
  (:use clojure.test useful.config))

(deftest reading
  (is (= '{size 1} (read-config "config1.clj")))

  (is (thrown-with-msg? java.io.FileNotFoundException #"Cannot find config resource config3.clj"
        (read-config "config3.clj")))

  (is (= nil (read-config "config3.clj" :optional? true))))

(deftest loading
  (is (= {:x [1 1]
          :y [1 1]}
         (load-config "config2.clj")))

  (is (thrown-with-msg? java.io.FileNotFoundException #"Cannot find config resource config3.clj"
        (load-config "config3.clj")))

  (is (= nil (load-config "config3.clj" :optional? true))))