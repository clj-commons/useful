(ns useful.io
  (:use [clojure.java.io :only [copy]])
  (:import [java.net URL URLConnection JarURLConnection]
           [java.io File FileInputStream]))

(defn resource-stream [name]
  (if-let [url (.findResource (.getClassLoader clojure.lang.RT) name)]
    (let [conn (.openConnection url)]
      (if (instance? JarURLConnection conn)
        (let [jar (cast JarURLConnection conn)]
          (.getInputStream jar))
        (FileInputStream. (File. (.getFile url)))))))

(defn extract-resource [name dest-dir]
  (if-let [s (resource-stream name)]
    (let [dest (File. dest-dir name)]
      (.mkdirs (.getParentFile dest))
      (copy s dest)
      dest)
    (throw (Exception. (format "unable to find %s on classpath" name)))))

(defmacro silently
  "Execute forms without printing to stdout or stderr."
  [& forms]
  (let [[opts & forms] (if (map? (first forms)) forms (cons {} forms))]
    `(if (:unless ~opts)
       (do ~@forms)
       (let [err# System/err, out# System/out
             null-stream# (java.io.PrintStream. (java.io.ByteArrayOutputStream.))
             null-writer# (java.io.OutputStreamWriter. null-stream#)]
         (binding [*err* null-writer#, *out* null-writer#]
           (System/setErr null-stream#)
           (System/setOut null-stream#)
           (let [result# (do ~@forms)]
             (System/setErr err#)
             (System/setOut out#)
             (if (:return-out opts) {:out (.toString null-stream#) :result result#} result#)))))))