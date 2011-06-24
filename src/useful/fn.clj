(ns useful.fn)

(defn decorate
  "Return a function f such that (f x) => [x (f1 x) (f2 x) ...]."
  [& fs]
  (apply juxt identity fs))

(defn annotate
  "A vector of [x (f1 x) (f2 x) ...]."
  [x & fs]
  ((apply decorate fs) x))

(defn transform-if
  "Returns a function that tests pred against its argument. If the result
is true, return (f arg); otherwise, return (f-not arg) (defaults to
identity)."
  ([pred f]
     (fn [x]
       (if (pred x) (f x) x)))
  ([pred f f-not]
     (fn [x]
       ((if (pred x) f f-not) x))))

(defn any
  "Takes a list of predicates and returns a new predicate that returns true if any do."
  [& preds]
  (fn [& args]
    (some #(apply % args) preds)))

(defn all
  "Takes a list of predicates and returns a new predicate that returns true if all do."
  [& preds]
  (fn [& args]
    (every? #(apply % args) preds)))

(defn thrush
  "Takes the first argument and applies the remaining arguments to it as functions from left to right.
   This tiny implementation was written by Chris Houser. http://blog.fogus.me/2010/09/28/thrush-in-clojure-redux"
  [& args]
  (reduce #(%2 %1) args))
