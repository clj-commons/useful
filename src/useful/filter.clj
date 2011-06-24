(ns useful.filter
  (:use [useful.fn :only [decorate]]))

(defn filter-keys-by-val
  "Returns all keys in map for which (pred value) returns true."
  [pred map]
  (when map
    (for [[key val] map :when (pred val)] key)))

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
