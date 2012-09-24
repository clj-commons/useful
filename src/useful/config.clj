(ns useful.config
  (:require [clojure.java.io :as io]))

(defn read-config [filename]
  (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
    (let [eof (Object.)
          forms (take-while (complement #{eof})
                            (repeatedly #(binding [*read-eval* false]
                                           (read in false eof))))]
      (if-let [error (cond (empty? forms) "No config data in %s"
                             (next forms) "Too many forms in %s")]
        (throw (IllegalArgumentException. (format error filename)))
        (first forms)))))

(defn load-config [filename]
  (eval (read-config filename)))
