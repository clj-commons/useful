(ns flatland.useful.map
  (:refer-clojure :exclude [update])
  (:use [flatland.useful.utils :only [map-entry pop-if]]
        [flatland.useful.fn :only [to-fix !]]))

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
  maps of keys to values or collections of alternating keys and values. If the first arg is
  a function, it will be used for merging duplicate values."
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
  (when m
    (into {}
          (for [[k v] m]
            (map-entry k (apply f v args))))))

(defn map-keys
  "Create a new map from m by calling function f on each key to get a new key."
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            (map-entry (apply f k args) v)))))

(defn map-vals-with-keys
  "Create a new map from m by calling function f, with two arguments (the key and value)
  to get a new value."
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            (map-entry k (apply f k v args))))))

(defn map-keys-and-vals
  "Create a new map from m by calling function f on each key & each value to get a new key & value"
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            (map-entry (apply f k args) (apply f v args))))))

(defn dissoc-in*
  "Dissociates a value in a nested associative structure, where ks is a sequence of keys and returns
  a new nested structure. If any resulting maps are empty, they will be removed from the new
  structure. This implementation was adapted from clojure.core.contrib, but the behavior is more
  correct if keys is empty."
  [m keys]
  (if-let [[k & ks] (seq keys)]
    (let [old (get m k ::sentinel)]
      (if-not (= old ::sentinel)
        (let [new (dissoc-in* old ks)]
          (if (seq new)
            (assoc m k new)
            (dissoc m k)))
        m))
    {}))

(defn assoc-in*
  "Associates a value in a nested associative structure, where ks is a sequence of keys and v is the
  new value and returns a new nested structure.  If any levels do not exist, hash-maps will be
  created. This implementation was adapted from clojure.core, but the behavior is more correct if
  keys is empty."
  [m keys v]
  (if-let [[k & ks] (seq keys)]
    (assoc m k (assoc-in* (get m k) ks v))
    v))

(defn update-in*
  "Updates a value in a nested associative structure, where ks is a sequence of keys and f is a
  function that will take the old value and any supplied args and return the new value, and returns
  a new nested structure. If any levels do not exist, hash-maps will be created. This implementation
  was adapted from clojure.core, but the behavior is more correct if keys is empty and unchanged
  values are not re-assoc'd."
  [m keys f & args]
  (if-let [[k & ks] (seq keys)]
    (let [old (get m k)
          new (apply update-in* old ks f args)]
      (if (identical? old new)
        m
        (assoc m k new)))
     (apply f m args)))

(defn update
  "Update a value for the given key in a map where f is a function that takes the previous value and
  the supplied args and returns the new value. Like update-in*, unchanged values are not
  re-assoc'd."
  [m key f & args]
  (apply update-in* m [key] f args))

(defn update-each
  "Update the values for each of the given keys in a map where f is a function that takes each
  previous value and the supplied args and returns a new value. Like update-in*, unchanged values
  are not re-assoc'd."
  [m keys f & args]
  (reduce (fn [m key]
            (apply update-in* m [key] f args))
          m keys))

(defn update-within
  "Like update-in*, but don't call f at all unless the map contains something at the given keyseq."
  [m keyseq f & args]
  (if (seq keyseq)
    (update-in* m (butlast keyseq)
                (fn [m*]
                  (let [k (last keyseq)]
                    (if (contains? m* k)
                      (apply update m* k f args)
                      m*))))
    (apply f m args)))

(letfn [(merge-in* [a b]
          (if (map? a)
            (merge-with merge-in* a b)
            b))]
  (defn merge-in
    "Merge multiple nested maps."
    [& args]
    (reduce merge-in* nil args)))

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
        (for [item (distinct coll)]
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
    (for [[key val] m :when (pred val)] key)))

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

(defn ordering-map
  "Create an empty map with a custom comparator that puts the given keys first, in the order
  specified. Other keys will be placed after the special keys, sorted by the default-comparator."
  ([key-order] (ordering-map key-order compare))
  ([key-order default-comparator]
     (let [indices (into {} (map-indexed (fn [i x] [x i]) key-order))]
       (sorted-map-by (fn [a b]
                        (if-let [a-idx (indices a)]
                          (if-let [b-idx (indices b)]
                            (compare a-idx b-idx)
                            -1)
                          (if-let [b-idx (indices b)]
                            1
                            (default-comparator a b))))))))
