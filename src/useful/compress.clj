(ns useful.compress
  (:import [java.util.zip DeflaterOutputStream InflaterInputStream]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [sun.misc BASE64Decoder BASE64Encoder]))

(defn smash [str]
  (let [out (ByteArrayOutputStream.)]
    (doto (DeflaterOutputStream. out)
      (.write (.getBytes str))
      (.finish))
    (.. (BASE64Encoder.)
        (encodeBuffer (.toByteArray out)))))

(defn unsmash [str]
  (let [bytes (.. (BASE64Decoder.) (decodeBuffer str))
        in    (ByteArrayInputStream. bytes)]
    (slurp (InflaterInputStream. in))))
