(ns useful.io
  (:use [clojure.java.io :only [reader]]
        [useful.ns :only [defalias]])
  (:import (java.io Reader PushbackReader)))

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
