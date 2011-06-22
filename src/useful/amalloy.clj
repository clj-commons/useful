(ns useful.amalloy)

(defn decorate
  "Return a function f such that (f x) => [x (f1 x) (f2 x) ...]."
  [& fs]
  (apply juxt identity fs))

(defn annotate
  "A vector of [x (f1 x) (f2 x) ...]."
  [x & fs]
  ((apply decorate fs) x))

(defn invoke
  "Like clojure.core/apply, but doesn't expand/splice the last
argument."
  ([f] (f))
  ([f x] (f x))
  ([f x & more] (apply f x more)))
