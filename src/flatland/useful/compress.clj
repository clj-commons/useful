(ns flatland.useful.compress
  (:import [java.util.zip DeflaterOutputStream InflaterInputStream]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [sun.misc BASE64Decoder BASE64Encoder]
           [java.util Base64]))

(defn smash [^String str]
  (let [out (ByteArrayOutputStream.)]
    (doto (DeflaterOutputStream. out)
      (.write (.getBytes str))
      (.finish))
    (-> (BASE64Encoder.)
        (.encodeBuffer (.toByteArray out)))))

(defn unsmash [^String str]
  (let [bytes (-> (BASE64Decoder.) (.decodeBuffer str))
        in    (ByteArrayInputStream. bytes)]
    (slurp (InflaterInputStream. in))))




(defn unsmash-new [^String s]
  (let [bytes (.decode (Base64/getDecoder) (subs s 0 (dec (count s))))
        in (ByteArrayInputStream. bytes)]
    (slurp (InflaterInputStream. in))))

(defn smash-new [^String s]
  (let [out (ByteArrayOutputStream.)]
    (doto (DeflaterOutputStream. out)
      (.write (.getBytes s))
      (.finish))
    (-> (.encodeToString (Base64/getEncoder) (.toByteArray out))
         (str (System/getProperty "line.separator")))))

