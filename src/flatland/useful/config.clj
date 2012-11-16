(ns flatland.useful.config
  (:require [clojure.java.io :as io]))

(defn read-config [filename & {:keys [optional]}]
  (let [resource (io/resource filename)]
    (if resource
      (with-open [in (java.io.PushbackReader. (io/reader resource))]
        (let [eof (Object.)
              forms (take-while (complement #{eof})
                                (repeatedly #(binding [*read-eval* false]
                                               (read in false eof))))]
          (if-let [error (cond (empty? forms) "No config data in %s"
                                 (next forms) "Too many forms in %s")]
            (throw (IllegalArgumentException. (format error filename)))
            (first forms))))
      (when-not optional
        (throw (java.io.FileNotFoundException. (format "Cannot find config resource %s" filename)))))))

(defn load-config [filename & args]
  (eval (apply read-config filename args)))
