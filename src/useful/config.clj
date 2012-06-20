(ns useful.config
  (:require [clojure.java.io :as io]))

(defn read-config [filename]
  (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
    (let [eof (Object.)
          forms (take-while (complement #{eof})
                            (repeatedly #(binding [*read-eval* false]
                                           (read in false eof))))]
      (cond (not (seq forms))
            (throw (IllegalArgumentException. (format "No config data in %s" filename)))

            (next forms)
            (throw (IllegalArgumentException. (format "Too many forms in %s" filename)))

            :else (first forms)))))

(defn load-config [filename]
  (eval (read-config filename)))
