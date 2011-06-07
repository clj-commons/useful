(ns useful.datatypes
  (:use [useful :only [position into-map update]])
  (:require [clojure.string :as s]))

(defn- normalize-field-name [field]
  (-> (name field)
      (s/replace #"_QMARK_" "?")
      (s/replace #"_"       "-")
      symbol))

(defn record-fields
  "Uses reflection to get the declared fields passed to the defrecord call for type. If called on a
   non-record, the behavior is undefined."
  [type]
  (->> (.getDeclaredFields (eval type))
       (remove #(java.lang.reflect.Modifier/isStatic (.getModifiers %)))
       (map #(symbol (normalize-field-name (.getName %))))))

(defmacro make-record
  "Construct a record given a pairs of lists and values. Mapping fields into constuctor arguments is
  done at compile time, so this is more efficient than creating an empty record and calling merge."
  [type & attrs]
  (let [fields (record-fields type)
        index  (position fields)
        vals   (reduce (fn [vals [field val]]
                         (if-let [i (index (normalize-field-name field))]
                           (assoc vals i val)
                           (assoc-in vals
                             [(index '--extmap) (keyword field)] val)))
                       (vec (repeat (count fields) nil))
                       (into-map attrs))]
    `(new ~type ~@vals)))

(defn- type-hint [binding]
  (and binding (.hasJavaClass binding) (.getJavaClass binding)))

(defmacro assoc-record
  "Assoc attrs into a record. Mapping fields into constuctor arguments is done at compile time,
   so this is more efficient than calling assoc on an existing record."
  [record & attrs]
  (let [type   (or (type-hint (get &env record)) (throw (Exception. "type hint required on record to use assoc-record")))
        fields (record-fields type)
        index  (position fields)
        vals   (reduce (fn [vals [field val]]
                         (if-let [i (index (normalize-field-name field))]
                           (assoc vals i val)
                           (assoc-in vals
                             [(index '--extmap) (keyword field)] val)))
                       (vec (map #(list (symbol (str "." %)) record) fields))
                       (into-map attrs))]
    `(new ~type ~@vals)))

(defmacro update-record
  "Construct a record given a list of forms like (update-fn record-field & args). Mapping fields
  into constuctor arguments is done at compile time, so this is more efficient than calling assoc on
  an existing record."
  [record & forms]
  (let [type   (or (type-hint (get &env record)) (throw (Exception. "type hint required on record to use update-record")))
        fields (record-fields type)
        index  (position fields)
        vals   (reduce (fn [vals [f field & args]]
                         (if-let [i (index (normalize-field-name field))]
                           (assoc vals
                             i (apply list f (get vals i) args))
                           (let [i (index '--extmap)]
                             (assoc vals
                               i (apply list `update (get vals i) (keyword field) args)))))
                       (vec (map #(list (symbol (str "." %)) record) fields))
                       forms)]
    `(new ~type ~@vals)))

(defmacro record-accessors
  "Defines optimized macro accessors using interop and typehints for all fields in the given records."
  [& types]
  `(do ~@(for [type  types
               field (record-fields type)]
           `(defmacro ~field [~'record]
              (list '~(symbol (str "." field))
                    (with-meta ~'record {:tag '~(symbol (.getName (eval type)))}))))))
