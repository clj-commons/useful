(ns useful.vec)

(defn avec-index
  "Given an association vector, a vector where each element is a vector of key
  and val, return the index in avec of the first element matching key."
  [avec key]
  (first (keep-indexed (fn [index [k v]]
                         (when (= k key)
                           index))
                       avec)))

(defn avec-find
  "Return the first element in association vector avec matching key."
  [avec key]
  (get avec (avec-index avec key)))

(defn avec-get
  "Return the first value in association vector avec matching key, or not-found."
  [avec key & [not-found]]
  (if-let [[_ val] (avec-find avec key)]
    val
    not-found))

(defn avec-keys
  "Return a sequence of all the keys in association vector avec."
  [avec]
  (map first avec))

(defn avec-vals
  "Return a sequence of all the vals in association vector avec."
  [avec]
  (map second avec))

(defn avec-assoc
  "Add the given key/val pairs to the association vector avec."
  ([avec key val]
     (if-let [index (avec-index avec key)]
       (assoc avec index [key val])
       (conj avec [key val])))
  ([avec key val & kvs]
     (let [avec (avec-assoc avec key val)]
       (apply avec-assoc avec kvs))))

(defn avec-dissoc
  "Remove the elements matching the given keys from the association vector avec."
  [avec & keys]
  (let [keys (set keys)]
    (vec (remove #(contains? keys (first %)) avec))))

(defn avec-update
  "Update the value matching key in the association vector avec by calling f with args."
  [avec key f & args]
  (if-let [index (avec-index avec key)]
    (let [val (second (get avec index))]
      (assoc avec index [key (apply f val args)]))
    (conj avec [key (apply f nil args)])))
