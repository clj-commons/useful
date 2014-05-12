(ns flatland.useful.io
  (:use [clojure.java.io :only [reader]]
        [flatland.useful.ns :only [defalias]]
        [flatland.useful.map :only [keyed]])
  (:require [clojure.tools.reader.edn :as edn])
  (:import (java.io Reader PushbackReader
                    ByteArrayInputStream ByteArrayOutputStream
                    DataOutputStream DataInputStream
                    RandomAccessFile)
           (java.nio.channels FileChannel$MapMode)))

(defprotocol PushbackFactory
  (^{:added "1.4"} pushback-reader [x] "Creates a PushbackReader from an object."))

(extend-protocol PushbackFactory
  PushbackReader
  (pushback-reader [this]
    this)

  Reader
  (pushback-reader [this]
    (PushbackReader. this))

  Object
  (pushback-reader [this]
    (pushback-reader (reader this))))

(defalias pbr pushback-reader)

(let [sentinel (Object.)
      valid? #(not (identical? % sentinel))]
  (defn read-seq
    "Read a lazy sequence of Clojure forms from an input reader."
    [in]
    (let [in (pushback-reader in)]
      (take-while valid?
                  (repeatedly #(edn/read {:eof sentinel} in))))))

(defn bytes->long
  "Read the first eight bytes of a byte-array and convert them to a Long using the standard
   network order (by delegating to DataInputStream)."
  [bytes]
  (-> bytes (ByteArrayInputStream.) (DataInputStream.) (.readLong)))

(defn long->bytes
  "Create an eight-byte array from a Long, using the standard
   network order (by delegating to DataOutputStream)."
  [long]
  (-> (ByteArrayOutputStream. 8)
      (doto (-> (DataOutputStream.) (.writeLong long)))
      (.toByteArray)))

(defn compare-bytes [^"[B" a ^"[B" b]
  (let [alen (alength a)
        blen (alength b)
        len (int (min alen blen))]
    (loop [idx (int 0)]
      (if (= idx len)
        (compare alen blen)
        (let [ai (long (aget a idx))
              bi (long (aget b idx))
              neg-ai? (neg? ai)
              diff (if (= neg-ai? (neg? bi))
                     (unchecked-subtract ai bi)
                     (if neg-ai? 1 -1))] ;; cannot subtract if signs are different
          (if (zero? diff)
            (recur (unchecked-inc-int idx))
            diff))))))

(defn mmap-file
  "Memory map a file. Returns a map containing a :buffer key which holds the
   mapped buffer and a :close key containing a function that, when called,
   closes the file."
  [^RandomAccessFile file]
  (let [channel (.getChannel file)
        buffer (.map channel FileChannel$MapMode/READ_WRITE 0 (.size channel))
        close #(.close file)]
    (keyed [buffer close])))
