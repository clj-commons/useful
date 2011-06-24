(ns useful.utils
  (:use [clojure.walk :only [walk]]
        [useful.amalloy :only [decorate]]
        [useful.amalloy.seq :only [alternates]]))

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

(def ^{:doc "The minimium value of vals, ignoring nils."
       :arglists '([& args])}
  or-min (ignoring-nils min))

(def ^{:doc "The maximium value of vals, ignoring nils."
       :arglists '([& args])}
  or-max (ignoring-nils max))

(defn conj-vec
  "Conj onto collection ensuring it is a vector."
  [coll item]
  (conj (vec coll) item))

(defn conj-set
  "Conj onto collection ensuring it is a set."
  [coll item]
  (conj (set coll) item))

(defn into-vec
  "Returns a new vector consisting of to-coll with all of the items of from-coll conjoined."
  [to-coll from-coll]
  (into (vec to-coll) from-coll))

(defn include?
  "Check if val exists in coll."
  [val coll]
  (some (partial = val) coll))

(defn split-vec
  "Split the given vector at the provided offsets using subvec. Supports negative offsets."
  [v & ns]
  (let [ns (map #(if (neg? %) (+ % (count v)) %) ns)]
    (lazy-seq
     (if-let [n (first ns)]
       (cons (subvec v 0 n)
             (apply split-vec
                    (subvec v n)
                    (map #(- % n) (rest ns))))
       (list v)))))

(defmacro if-ns
  "Try to load a namespace reference. If successful, evaluate then-form otherwise evaluate else-form."
  [ns-reference then-form & [else-form]]
  `(try (ns ~(ns-name *ns*) ~ns-reference)
        (eval '~then-form)
        (catch Exception e#
          (when-not (or (instance? java.io.FileNotFoundException    e#)
                        (instance? java.lang.ClassNotFoundException e#))
            (printf "%s: %s %s" (.getName (class e#)) (.getMessage e#) '~ns-reference))
          (eval '~else-form))))

(defn tap
  "Call f on obj, presumably with side effects, then return obj. Useful for debugging when
   you want to print an object inline. e.g. (tap println foo)"
  [f obj]
  (doto obj f))

(defn adjoin
  "Merge two data structures by combining the contents. For maps, merge recursively by
  adjoining values with the same key. For collections, combine the right and left using
  into or conj. If the left value is a set and the right value is a map, the right value
  is assumed to be an existence map where the value determines whether the key is in the
  merged set. This makes sets unique from other collections because items can be deleted
  from them."
  [left right]
  (cond (map? left)
        (merge-with adjoin left right)

        (and (set? left) (map? right))
        (reduce (fn [set [k v]] ((if v conj disj) set k))
                left right)

        (coll? left)
        ((if (coll? right) into conj) left right)

        :else right))

(defmacro while-let
  "Repeatedly executes body, which presumably has side-effects, while let binding is not false."
  [bindings & body]
  (let [[form test] bindings]
    `(loop [~form ~test]
       (when ~form
         ~@body
         (recur ~test)))))

(defn queue
  "Create an empty persistent queue or a persistent queue from a sequence."
  ([]    clojure.lang.PersistentQueue/EMPTY)
  ([seq] (into (queue) seq)))

(defmacro absorb
  "Thread val through form if val is not nil."
  [val form]
  `(let [v# ~val]
     (when-not (nil? v#)
       (-> v# ~form))))

(defmacro defm [& defn-args]
  "Define a function with memoization. Takes the same arguments as defn."
  `(doto (defn ~@defn-args)
     (alter-var-root #(with-meta (memoize %) (meta %)))))

(defn memoize-deref
  "Returns a memoized version a non-referentially transparent function, calling deref on each
   provided var (or ref or atom) and using that in the cache key to prevent cross-contamination if
   any of the values change."
  [vars f]
  (let [mem (memoize
             (fn [args vals]
               (apply f args)))]
    (fn [& args]
      (mem args (doall (map deref vars))))))

(defmacro cond-let
  "An implementation of cond-let that is as similar as possible to if-let. Takes multiple
   test-binding/then-form pairs and evalutes the form if the binding is true. Also supports
   :else in the place of test-binding and always evaluates the form in that case.

   Example:
   (cond-let [b (bar 1 2 3)] (println :bar b)
             [f (foo 3 4 5)] (println :foo f)
             [b (baz 6 7 8)] (println :baz b)
             :else           (println :no-luck))"
  [test-binding then-form & more]
  (let [test-binding (if (= :else test-binding) `[t# true] test-binding)
        else-form    (when (seq more) `(cond-let ~@more))]
    `(if-let ~test-binding
       ~then-form
       ~else-form)))

(defmacro let-if
  "Choose a set of bindings based on the result of a conditional test.

   Example:
   (let-if (even? a)
           [b (bar 1 2 3) (baz 1 2 3)
            c (foo 1)     (foo 3)]
     (println (combine b c)))"
  [test bindings & forms]
  (let [[names thens elses] (alternates 3 bindings)]
    `(if ~test
       (let [~@(interleave names thens)] ~@forms)
       (let [~@(interleave names elses)] ~@forms))))

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

(defn position
  "Returns a map from item to the position of its first occurence in coll."
  [coll]
  (into {} (map-indexed (fn [idx val] [val idx])
                        (reverse coll)))) ; so earlier values clobber later ones

(defn map-to
  "Returns a map from each item in coll to f applied to that item."
  [f coll]
  (into {}
        (for [item coll]
          [item (f item)])))

(defn index-by
  "Returns a map from the result of calling f on each item in coll to that item."
  [f coll]
  (into {}
        (for [item coll]
          [(f item) item])))

(defn pluralize
  "Return a pluralized phrase, appending an s to the singular form if no plural is provided.
   For example:nn
     (plural 5 \"month\") => \"5 months\"
     (plural 1 \"month\") => \"1 month\"
     (plural 1 \"radius\" \"radii\") => \"1 radius\"
     (plural 9 \"radius\" \"radii\") => \"9 radii\""
  [num singular & [plural]]
  (let [plural (or plural (str singular "s"))]
    (str num " " (if (= 1 num) singular plural))))

(defn syntax-quote ;; from leiningen.core/unquote-project
  "Syntax quote the given form, wrapping all seqs and symbols in quote."
  [form]
  (walk (fn [form]
          (cond (and (seq? form) (= `unquote (first form))) (second form)
                (or (seq? form) (symbol? form)) (list 'quote form)
                :else (syntax-quote form)))
        identity
        form))
