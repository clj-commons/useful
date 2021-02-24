(ns flatland.useful.compress
  (:import
    [java.io ByteArrayOutputStream ByteArrayInputStream]
    [java.util Base64]
    [java.util.zip DeflaterOutputStream InflaterInputStream] ))

(defn smash [str]
  (let [out (ByteArrayOutputStream.)]
    (doto (DeflaterOutputStream. out)
      (.write (.getBytes str))
      (.finish))
    (-> (Base64/getEncoder)
      (.encode (.toByteArray out)))))

(defn unsmash [str]
  (let [bytes (-> (Base64/getDecoder) (.decode str))
        in    (ByteArrayInputStream. bytes)]
    (slurp (InflaterInputStream. in))))

