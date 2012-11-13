(ns flatland.useful.datatypes
  (:use [flatland.useful.map :only [position into-map update]]
        [flatland.useful.utils :only [invoke]]
        [flatland.useful.fn :only [fix]])
  (:require [clojure.string :as s])
  (:import (java.lang.reflect Field)
           (clojure.lang Compiler$LocalBinding)))

(defn as-int [x]
  (condp invoke x
    integer? x
    string? (Integer/parseInt x)
    float? (int x)
    nil))

(let [munge-ops [["?" "_QMARK_"]
                 ["!" "_BANG_"]
                 ["-" "_"]]
      munger (fn [f]
               (fn [field]
                 (symbol (reduce (fn [^String s op]
                                   (let [[from to] (f op)]
                                     (.replaceAll s (java.util.regex.Pattern/quote from) to)))
                                 (name field)
                                 munge-ops))))]
  (def clj->java (munger seq))
  (def java->clj (munger rseq)))

(defn- ^Class coerce-class
  "Get a Class object from either a Symbol (by resolving it) or a Class."
  [type]
  (fix type symbol? resolve))

(defn- record-fields
  "Uses reflection to get the declared fields passed to the defrecord call for type. If called on a
   non-record, the behavior is undefined."
  ([type]
     (record-fields type clj->java))
  ([type lang]
     (->> (.getDeclaredFields (coerce-class type))
          (remove #(java.lang.reflect.Modifier/isStatic (.getModifiers ^Field %)))
          (remove #(let [name (.getName ^Field %)]
                     (and (not (#{"__extmap" "__meta"} name))
                          (.startsWith name "__"))))
          (map #(lang (.getName ^Field %))))))

(defmacro make-record
  "Construct a record given a pairs of lists and values. Mapping fields into constuctor arguments is
  done at compile time, so this is more efficient than creating an empty record and calling merge."
  [type & attrs]
  (let [fields (record-fields type clj->java)
        index  (position fields)
        vals   (reduce (fn [vals [field val]]
                         (if-let [i (index (clj->java field))]
                           (assoc vals i val)
                           (assoc-in vals
                             [(index '__extmap) (keyword field)] val)))
                       (vec (repeat (count fields) nil))
                       (into-map attrs))]
    `(new ~type ~@vals)))

(defn- type-hint [form &env fn-name]
  (or (:tag (meta form))
      (let [^Compiler$LocalBinding binding (get &env form)]
        (and binding (.hasJavaClass binding) (.getJavaClass binding)))
      (throw (Exception. (str "type hint required on record to use " fn-name)))))

(defmacro assoc-record
  "Assoc attrs into a record. Mapping fields into constuctor arguments is done at compile time,
   so this is more efficient than calling assoc on an existing record."
  [record & attrs]
  (let [r      (gensym 'record)
        type   (type-hint record &env 'assoc-record)
        fields (record-fields type clj->java)
        index  (position fields)
        vals   (reduce (fn [vals [field val]]
                         (if-let [i (index (clj->java field))]
                           (assoc vals i val)
                           (assoc-in vals
                             [(index '__extmap) (keyword field)] val)))
                       (vec (map #(list '. r %) fields))
                       (into-map attrs))]
    `(let [~r ~record]
       (new ~type ~@vals))))

(defmacro update-record
  "Construct a record given a list of forms like (update-fn record-field & args). Mapping fields
  into constuctor arguments is done at compile time, so this is more efficient than calling assoc on
  an existing record."
  [record & forms]
  (let [r      (gensym 'record)
        type   (type-hint record &env 'update-record)
        fields (record-fields type clj->java)
        index  (position fields)
        vals   (reduce (fn [vals [f field & args]]
                         (if-let [i (index (clj->java field))]
                           (assoc vals
                             i `(~f ~(get vals i) ~@args))
                           (let [i (index '__extmap)]
                             (assoc vals
                               i `(update ~(get vals i) ~(keyword field) ~@args)))))
                       (vec (map #(list '. r %) fields))
                       forms)]
    `(let [~r ~record]
       (new ~type ~@vals))))

(defmacro record-accessors
  "Defines optimized macro accessors using interop and typehints for all fields in the given records."
  [& types]
  `(do ~@(for [type  types
               :let [tag (symbol (.getName (coerce-class type)))]
               field (record-fields type clj->java)
               :when (not (.startsWith (name field) "__"))]
           `(defmacro ~(java->clj field) [~'record]
              (with-meta
                (list '. (with-meta ~'record {:tag '~tag})
                      '~field)
                (meta ~'&form))))))
