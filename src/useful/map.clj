(ns useful.map
  (:use [useful.utils :only [map-entry]]))

(defmacro assoc-if
  "Create mapping from keys to values in map if test returns true."
  [map test & kvs]
  `(if ~test
     (assoc ~map ~@kvs)
     ~map))

(let [transforms {:keys keyword
                  :strs str
                  :syms identity}]
  (defmacro keyed
      "Create a map in which, for each symbol S in vars, (keyword S) is a
  key mapping to the value of S in the current scope. If passed an optional
  :strs or :syms first argument, use strings or symbols as the keys instead."
    ([vars] `(keyed :keys ~vars))
    ([key-type vars]
       (let [transform (comp (partial list `quote)
                             (transforms key-type))]
         (into {} (map (juxt transform identity) vars))))))

(defn assoc-or
  "Create mapping from each key to val in map only if existing val is nil."
  ([map key val]
     (if (nil? (map key))
       (assoc map key val)
       map))
  ([map key val & kvs]
     (let [map (assoc-or map key val)]
       (if kvs
         (recur map (first kvs) (second kvs) (nnext kvs))
         map))))

(defn into-map
  "Convert a list of heterogeneous args into a map. Args can be alternating keys and values,
   maps of keys to values or collections of alternating keys and values."
  [& args]
  (loop [args args map {}]
    (if (empty? args)
      map
      (let [arg  (first args)
            args (rest args)]
       (condp #(%1 %2) arg
         nil?  (recur args map)
         map?  (recur args (merge map arg))
         coll? (recur (into args (reverse arg)) map)
         (recur (rest args) (assoc map arg (first args))))))))

(defn map-vals
  "Create a new map from m by calling function f on each value to get a new value."
  [f m]
  (into {}
        (for [[k v] m]
          (map-entry k (f v)))))

(defn map-vals-with-keys
  "Create a new map from m by calling function f, with two arguments (the key and value)
  to get a new value."
  [f m]
  (into {}
        (for [[k v] m]
          (map-entry k (f k v)))))

(defn map-keys-and-vals
  "Create a new map from m by calling function f on each key & each value to get a new key & value"
  [f m]
  (into {}
        (for [[k v] m]
          (map-entry (f k) (f v)))))

(defn update
  "Update value in map where f is a function that takes the old value and the supplied args and
  returns the new value. For efficiency, Do not change map if the old value is the same as the new
  value. If key is sequential, update all keys in the sequence with the same function."
  [map key f & args]
  (if (sequential? key)
    (reduce #(apply update %1 %2 f args) map key)
    (let [old (get map key)
          new (apply f old args)]
      (if (= old new) map (assoc map key new)))))

(defn merge-in
  "Merge two nested maps."
  [left right]
  (if (map? left)
    (merge-with merge-in left right)
    right))

(defn update-in!
  "'Updates' a value in a nested associative structure, where ks is a sequence of keys and
  f is a function that will take the old value and any supplied args and return the new
  value, and returns a new nested structure. The associative structure can have transients
  in it, but if any levels do not exist, non-transient hash-maps will be created."
  [m [k & ks] f & args]
  (let [assoc (if (instance? clojure.lang.ITransientCollection m) assoc! assoc)
        val (get m k)]
    (assoc m k (if ks
                 (apply update-in! val ks f args)
                 (apply f val args)))))

(defn assoc-in!
  "Associates a value in a nested associative structure, where ks is a sequence of keys
  and v is the new value and returns a new nested structure. The associative structure
  can have transients in it, but if any levels do not exist, non-transient hash-maps will
  be created."
  [m ks v]
  (update-in! m ks (constantly v)))

(defn map-to
  "Returns a map from each item in coll to f applied to that item."
  [f coll]
  (into {}
        (for [item coll]
          (map-entry item (f item)))))

(defn index-by
  "Returns a map from the result of calling f on each item in coll to that item."
  [f coll]
  (into {}
        (for [item coll]
          (map-entry (f item) item))))

(defn position
  "Returns a map from item to the position of its first occurence in coll."
  [coll]
  (into {} (reverse (map-indexed (fn [idx val] (map-entry val idx)) coll))))

(defn filter-keys-by-val
  "Returns all keys in map for which (pred value) returns true."
  [pred map]
  (when map
    (set (for [[key val] map :when (pred val)] key))))

(defn remove-keys-by-val
  "Returns all keys of map for which (pred value) returns false."
  [pred map]
  (filter-keys-by-val (complement pred) map))

(defn filter-vals
  "Returns a map that only contains values where (pred value) returns true."
  [pred map]
  (when map
    (select-keys map (filter-keys-by-val pred map))))

(defn remove-vals
  "Returns a map that only contains values where (pred value) returns false."
  [pred map]
  (filter-vals (complement pred) map))

(defn filter-keys
  "Returns a map that only contains keys where (pred key) returns true."
  [pred map]
  (when map
    (select-keys map (filter pred (keys map)))))

(defn remove-keys
  "Returns a map that only contains keys where (pred key) returns false."
  [pred map]
  (filter-keys (complement pred) map))
