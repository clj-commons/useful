(ns useful.datatypes
  (:use [useful :only [position into-map]])
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
  done an compile time, so this is more efficient than creating an empty record and calling merge."
  [type & attrs]
  (let [fields  (record-fields type)
        index   (position fields)
        vals    (reduce
                 (fn [vals [field val]]
                   (if-let [i (index (normalize-field-name field))]
                     (assoc vals i val)
                     (throw (Exception. (format "invalid field name %s for record %s" field type)))))
                 (vec (repeat (count fields) nil))
                 (into-map attrs))]
    `(new ~type ~@vals)))

(defmacro update-record
  "Construct a record given a list of forms like (update-fn record-field & args). Mapping fields
  into constuctor arguments is done an compile time, so this is more efficient than calling assoc on
  an existing record."
  [record & forms]
  (let [binding (get &env record)
        type    (or (and binding (.getJavaClass binding))
                    (throw (Exception. "type hint required on record to use update-record")))
        fields  (record-fields type)
        index   (position fields)
        vals    (reduce
                 (fn [vals [f field & args]]
                   (let [i (index (normalize-field-name field))]
                     (assoc vals
                       i (cons f (cons (get vals i) args)))))

                 (vec (map #(list (symbol (str "." %)) record) fields))
                 forms)]
    `(new ~type ~@vals)))
