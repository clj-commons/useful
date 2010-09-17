(ns useful.io
  (:use [clojure.java.io :only [copy]])
  (:import [java.net URL URLConnection JarURLConnection]
           [java.io File FileInputStream]
           [clojure.lang Atom]))

(defmacro multi-outstream [var]
  (letfn [(outs [val] (if (instance? Atom val) (first @val) val))]
    `(java.io.PrintStream.
      (proxy [java.io.BufferedOutputStream] [nil]
        (write
          ([b#]           (.write (~outs ~var) b#))
          ([b# off# len#] (.write (~outs ~var) b# off# len#)))
        (flush [] (.flush (~outs ~var)))))))

(defmacro with-outstream [bindings & forms]
  `(do (doseq [[var# outs#] (partition 2 ~bindings)]
         (swap! var# conj outs#))
       (binding ~bindings ~@forms)
       (doseq [[var# outs#] (partition 2 ~bindings)]
         (doall (swap! var# (partial remove #(= outs# %)))))))

(defn default-outstream-push [outs default]
  (swap! outs conj default))

(defn default-outstream-pop [outs default]
  (doall (swap! outs (partial remove #(= default %)))))

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
