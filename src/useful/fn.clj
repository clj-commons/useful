(ns useful.fn)

(def ! complement)

(defn decorate
  "Return a function f such that (f x) => [x (f1 x) (f2 x) ...]."
  [& fs]
  (apply juxt identity fs))

(defn annotate
  "A vector of [x (f1 x) (f2 x) ...]."
  [x & fs]
  ((apply decorate fs) x))

(defn to-fix
  "Returns a function taking pred/transform pairs. Calls each pred in order against its argument,
  calling transform if pred returns true. If a final else clause is provided, it will be used to
  transform an argument that doesn't match any pred, otherwise the argument is returned unchanged."
  [& clauses]
  (fn [x]
    (loop [[pred? transform & clauses] clauses]
      (cond (nil? pred?)     x
            (nil? transform) (pred? x)
            (pred? x)        (transform x)
            :else            (recur clauses)))))

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

(defn ignoring-nils
  "Create a new version of a function which ignores all nils in its arguments:
((ignoring-nils +) 1 nil 2 3 nil) yields 6."
  [f]
  (fn
    ([])
    ([a] (f a))
    ([a b]
       (cond (nil? b) (f a)
             (nil? a) (f b)
             :else (f a b)))
    ([a b & more]
       (when-let [items (seq (remove nil? (list* a b more)))]
         (apply f items)))))