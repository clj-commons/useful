(ns flatland.useful.destructure
  (:use flatland.useful.debug)
  (:use flatland.useful.debug))

(declare destructure')

(defn handle-as [k v prefix lookup refine]
  (let [bind (gensym prefix)]
    {:as bind, :bindings `[~bind ~v
                           ~@(refine bind)
                           ~@(when-let [as (lookup k)]
                               (destructure' as bind))]}))

(defn map-bindings [as or]
  (let [special-handlers {:keys keyword, :strs name, :syms (partial list `quote)}
        ors (into {} (for [e or]
                       [(key e) [(val e)]]))]
    (fn [e]
      (let [k (key e), v (val e)]
        (if-let [lookup (special-handlers k)]
          (apply concat (for [kw v]
                          [kw `(get ~as ~(lookup kw) ~@(ors kw))]))
          (destructure' k `(get ~as ~v ~@(ors k))))))))

(defn destructure-map [k v]
  (let [state (handle-as k v "map__" :as (fn [sym]
                                           `[~sym (if (seq? ~sym)
                                                    (clojure.lang.PersistentHashMap/create
                                                     ~(with-meta sym {:tag `clojure.lang.ISeq}))
                                                    ~sym)]))
        as (:as state)
        or (:or k)]
    (into (:bindings state)
          (mapcat (map-bindings as or) k))))

(defn destructure-vector [k v]
  (let [state (handle-as k v "vec__"
                         #(second (drop-while (complement #{:as}) %)) (constantly nil))]
    (loop [binds (:bindings state), names k, curr (:as state)]
      (if (or (empty? names) (= :as (first names)))
        binds
        (let [bind (first names)]
          (if (= '& bind)
            (if (and (nnext names) (not= :as (nth names 2)))
              (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
              (into binds (destructure' (second names) curr)))
            (let [new-name (gensym "vec__")]
              (recur (reduce into binds
                             [(destructure' bind `(first ~curr))
                              (when (and (next names) (not= :as (second names)))
                                [new-name `(next ~curr)])])
                     (next names)
                     new-name))))))))

(defn destructure' [k v]
  (cond (symbol? k) [k v]
        (map? k) (destructure-map k v)
        (vector? k) (destructure-vector k v)
        :else (throw (new Exception (str "Unsupported binding form: " k)))))

(defn destructure [binds]
  (vec (mapcat (partial apply destructure') (partition 2 binds))))