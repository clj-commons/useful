(ns flatland.useful.string
  (:require [clojure.string :as s]))

(defn camelize [string]
  (s/replace string
             #"[-_](\w)"
             (comp s/upper-case second)))

(defn classify [string]
  (apply str (map s/capitalize
                  (s/split string #"[-_]"))))

(letfn [(from-camel-fn [separator]
          (fn [string]
            (-> string
                (s/replace #"^[A-Z]+" s/lower-case)
                (s/replace #"_?([A-Z]+)"
                           (comp (partial str separator)
                                 s/lower-case second))
                (s/replace #"-|_" separator))))]

  (def dasherize (from-camel-fn "-"))
  (def underscore (from-camel-fn "_")))

(defn pluralize
  "Return a pluralized phrase, appending an s to the singular form if no plural is provided.
  For example:
     (pluralize 5 \"month\") => \"5 months\"
     (pluralize 1 \"month\") => \"1 month\"
     (pluralize 1 \"radius\" \"radii\") => \"1 radius\"
     (pluralize 9 \"radius\" \"radii\") => \"9 radii\""
  [num singular & [plural]]
  (str num " " (if (= 1 num) singular (or plural (str singular "s")))))

(defn substring-after
  "Find the part of the string s which comes after the last instance of delim."
  [^String delim]
  (fn [^String s]
    (let [idx (.lastIndexOf s delim)]
      (if (neg? idx)
        s ;; no match
        (subs s (+ (.length delim) idx))))))

(defn substring-before
  "Find the part of the string s which comes before the first instance of delim."
  [^String delim]
  (fn [^String s]
    (let [idx (.indexOf s delim)]
      (if (= -1 idx)
        s
        (subs s 0 idx)))))

(defn indexes-of
  "Returns a lazy seq of the the indexes within s of occurrences of the given substring,
   starting at the specified index"
  ([s substr]
    (indexes-of s substr 0))
  ([^String s ^String substr ^Integer index]
    (lazy-seq
      (let [next-index (.indexOf s substr index)]
        (when-not (or (neg? next-index)
                      (>= index (count s)))
          (cons
            next-index
            (indexes-of s substr (inc next-index))))))))
