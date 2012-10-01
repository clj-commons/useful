(ns useful.deftype
  (:use [useful.experimental.delegate :only [parse-deftype-specs emit-deftype-specs]]
        [useful.map :only [merge-in]])
  (:require [clojure.string :as s])
  (:import (clojure.lang IObj MapEntry IPersistentVector
                         IPersistentMap APersistentMap MapEquivalence)
           (java.util Map Map$Entry)))

;; to define a new map type, you still must provide:
;; - IPersistentMap:
;;   - (.count this)
;;   - (.valAt this k not-found)
;;   - (.empty this)
;;   - (.assoc this k v)
;;   - (.without this k)
;;   - (.seq this)
;;   - recommended but not required: (.entryAt this k)
;; - IObj
;;   - (.meta this)
;;   - (.withMeta this m)

(defmacro defmap [name fields & specs]
  `(deftype ~name ~fields
     ~@(emit-deftype-specs
         (->> (parse-deftype-specs specs)
              (merge-in (parse-deftype-specs
                         `(java.util.Map
                           (size [this#]
                                 (count this#))
                           (containsKey [this# k#]
                                        (contains? this# k#))
                           (isEmpty [this#]
                                    (empty? this#))
                           (keySet [this#]
                                   (set (keys this#)))
                           (values [this#]
                                   (vals this#))
                           (get [this# k#]
                                (get this# k#))
                           (containsValue [this# v#]
                                          (boolean (seq (filter #(= % v#) (vals this#)))))

                           Object
                           (toString [this#]
                                     (str "{" (s/join ", " (for [[k# v#] this#] (str k# " " v#))) "}"))
                           (equals [this# other#]
                                   (= this# other#))
                           (hashCode [this#]
                                     (APersistentMap/mapHash this#))

                           clojure.lang.IFn
                           (invoke [this# k#]
                                   (get this# k#))
                           (invoke [this# k# not-found#]
                                   (get this# k# not-found#))

                           MapEquivalence

                           IPersistentMap
                           (equiv [this# other#]
                                  (and (instance? Map other#)
                                       (or (instance? MapEquivalence other#)
                                           (not (instance? IPersistentMap other#)))
                                       (= (count this#) (count other#))
                                       (every? (fn [e#]
                                                 (let [k# (key e#)
                                                       o# ^Map other#]
                                                   (and (.containsKey o# k#)
                                                        (= (.get o# k#) (val e#)))))
                                               (seq this#))))
                           (entryAt [this# k#]
                                    (let [not-found# (Object.)
                                          v# (get this# k# not-found#)]
                                      (when (not= v# not-found#)
                                        (MapEntry. k# v#))))
                           (valAt [this# k#]
                                  (get this# k# nil))
                           (cons [this# obj#]
                                 (condp instance? obj#
                                   Map$Entry (assoc this# (key obj#) (val obj#))
                                   IPersistentVector (if (= 2 (count obj#))
                                                       (assoc this# (nth obj# 0) (nth obj# 1))
                                                       (throw (IllegalArgumentException.
                                                               "Vector arg to map conj must be a pair")))
                                   (reduce (fn [m# e#]
                                             (assoc m# (key e#) (val e#)))
                                           this# obj#)))
                           (iterator [this#]
                                     (clojure.lang.SeqIterator. (seq this#))))))))))

(defmap AList [entries meta]
  IPersistentMap
  (count [this]
         (count entries))
  (valAt [this k not-found]
         (if-let [e (find this k)]
           (val e)
           not-found))
  (entryAt [this k]
           (first (filter #(= k (key %)) entries)))
  (empty [this]
         (AList. () meta))
  (seq [this]
       (seq entries))
  (assoc [this k v]
    (AList. (conj entries (MapEntry. k v)) meta))
  (without [this k]
           (AList. (->> entries (remove #(= k (key %)))
                        (apply list))
                   meta))

  IObj
  (meta [this]
        meta)
  (withMeta [this meta]
            (AList. entries meta)))

(defn alist [& kvs]
  (AList. (apply list (map vec (partition 2 kvs))) nil))
