(ns useful.map
  (:use [useful.utils :only [map-entry pop-if]]
        [useful.fn :only [to-fix !]]))

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
  ([m key val]
     (if (nil? (m key))
       (assoc m key val)
       m))
  ([m key val & kvs]
     (let [m (assoc-or m key val)]
       (if kvs
         (recur m (first kvs) (second kvs) (nnext kvs))
         m))))

(defn into-map
  "Convert a list of heterogeneous args into a map. Args can be alternating keys and values,
   maps of keys to values or collections of alternating keys and values."
  [& args]
  (let [[args combine] (pop-if (apply list args) fn? (fn [_ x] x))]
    (loop [args args m {}]
      (if (empty? args)
        m
        (let [arg  (first args)
              args (rest args)]
          (condp #(%1 %2) arg
            nil?  (recur args m)
            map?  (recur args (merge-with combine m arg))
            coll? (recur (into args (reverse arg)) m)
            (recur (conj (rest args) {arg (first args)}) m)))))))

(defn map-vals
  "Create a new map from m by calling function f on each value to get a new value."
  [m f & args]
  (into {}
        (for [[k v] m]
          (map-entry k (apply f v args)))))

(defn map-vals-with-keys
  "Create a new map from m by calling function f, with two arguments (the key and value)
  to get a new value."
  [m f & args]
  (into {}
        (for [[k v] m]
          (map-entry k (apply f k v args)))))

(defn map-keys-and-vals
  "Create a new map from m by calling function f on each key & each value to get a new key & value"
  [m f & args]
  (into {}
        (for [[k v] m]
          (map-entry (apply f k args) (apply f v args)))))

(defn update
  "Update a value for the given key in a map where f is a function that takes the
  previous value and the supplied args and returns the new value."
  [m key f & args]
  (if-let [val (apply f (get m key) args)]
    (assoc m key val)
    (dissoc m key)))

(defn update-each
  "Update the values for each of the given keys in a map where f is a function that takes
  each previous value and the supplied args and returns a new value."
  [m keys f & args]
  (reduce (fn [m key]
            (apply update m key f args))
          m keys))

(defn merge-in
  "Merge multiple nested maps."
  [& args]
  (if (map? (first args))
    (apply merge-with merge-in args)
    (last args)))

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
  [pred m]
  (when m
    (set (for [[key val] m :when (pred val)] key))))

(defn remove-keys-by-val
  "Returns all keys of map for which (pred value) returns false."
  [pred m]
  (filter-keys-by-val (complement pred) m))

(defn filter-vals
  "Returns a map that only contains values where (pred value) returns true."
  [m pred]
  (when m
    (select-keys m (filter-keys-by-val pred m))))

(defn remove-vals
  "Returns a map that only contains values where (pred value) returns false."
  [m pred]
  (filter-vals m (complement pred)))

(defn filter-keys
  "Returns a map that only contains keys where (pred key) returns true."
  [m pred]
  (when m
    (select-keys m (filter pred (keys m)))))

(defn remove-keys
  "Returns a map that only contains keys where (pred key) returns false."
  [m pred]
  (filter-keys m (complement pred)))

(defn multi-map
  "Takes a map with keys and values that can be sets or individual objects and returns a map from
  objects to sets. Used to create associations between two sets of objects."
  [m]
  (apply merge-with into
         (for [entry m, :let [[ks vs] (map (to-fix (! set?) hash-set) entry)]
               k ks]
           {k vs})))
