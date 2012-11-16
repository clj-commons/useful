(ns flatland.useful.io
  (:use [clojure.java.io :only [reader]]
        [flatland.useful.ns :only [defalias]])
  (:import (java.io Reader PushbackReader
                    ByteArrayInputStream ByteArrayOutputStream
                    DataOutputStream DataInputStream)))

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
                  (repeatedly #(read in false sentinel))))))

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
    (loop [idx 0]
      (if (= idx len)
        (compare alen blen)
        (let [diff (unchecked-subtract (aget a idx) (aget b idx))]
          (if (zero? diff)
            (recur (unchecked-inc-int idx))
            diff))))))
