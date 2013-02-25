(ns flatland.useful.io-test
  (:use flatland.useful.io clojure.test)
  (:import (java.io StringReader RandomAccessFile)))

(deftest test-read-seq
  (let [forms '(this (is) #(100 %) ~of a [long, [complicated, [nested]]] {:quoted #{form}})
        form-str (with-out-str (doseq [form forms]
                                 (prn form)))]
    (is (= forms (read-seq (StringReader. form-str))))))

(deftest test-bytes-and-longs
  (are [x bs] (and (= x (bytes->long (into-array Byte/TYPE (map byte bs))))
                   (= bs (seq (long->bytes x))))
       10    [0 0 0 0 0 0 0 10]
       255   [0 0 0 0 0 0 0 -1]
       256   [0 0 0 0 0 0 1 0]
       65540 [0 0 0 0 0 1 0 4]))

(deftest test-mmap-file
  (let [{:keys [buffer close]} (mmap-file (RandomAccessFile. "project.clj" "rw"))
        a (byte-array (.capacity buffer))]
    (is (= (slurp "project.clj")
           (do (.get buffer a 0 (alength a))
               (apply str (map char a)))))
    (close)))
