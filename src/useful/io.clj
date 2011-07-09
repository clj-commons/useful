(ns useful.io
  (:use [clojure.java.io :only [copy]])
  (:import [java.net URL URLConnection JarURLConnection]
           [java.io File FileInputStream PrintStream]
           [clojure.lang IDeref]))

(defn resource-stream [name]
  (if-let [url (.findResource (.getClassLoader clojure.lang.RT) name)]
    (let [conn (.openConnection url)]
      (if (instance? JarURLConnection conn)
        (.getInputStream ^JarURLConnection conn)
        (FileInputStream. (File. (.getFile url)))))))

(defn extract-resource [name dest-dir]
  (if-let [s (resource-stream name)]
    (let [dest (File. dest-dir name)]
      (.mkdirs (.getParentFile dest))
      (copy s dest)
      dest)
    (throw (Exception. (format "unable to find %s on classpath" name)))))
