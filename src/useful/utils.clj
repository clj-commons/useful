(ns useful.utils
  (:use [clojure.walk :only [walk]]
        [useful.fn :only [decorate ignoring-nils fix]]))

(defn invoke
  "Like clojure.core/apply, but doesn't expand/splice the last argument."
  ([f] (f))
  ([f x] (f x))
  ([f x & more] (apply f x more)))

(defmacro verify
  "Raise exception unless test returns true."
  [test exception]
  `(when-not ~test
     (throw (fix ~exception string? #(Exception. ^String %)))))

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
          (when-not (some #(instance? % e#) [java.io.FileNotFoundException
                                             java.lang.ClassNotFoundException])
            (printf "%s: %s %s" (.getName (class e#)) (.getMessage e#) '~ns-reference))
          (eval '~else-form))))

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

(defn pop-if
  "Pop item off the given stack if (pred? item) returns true, returning both the item and the
  modified stack. If (pred? item) is false, return nil or the optional default value."
  [stack pred? & [default]]
  (let [[peek pop] (if (instance? clojure.lang.IPersistentStack stack)
                     [peek pop]
                     [first rest])
        item (peek stack)]
    (if (pred? item)
      [(pop stack) item]
      [stack default])))

(defmacro with-adjustments
  "Create new bindings for binding args, by applying adjustment
  function to current values of bindings."
  [adjustment bindings & body]
  (let [bindings (vec bindings)]
    `(let [~bindings (map ~adjustment ~bindings)]
       ~@body)))

(defn queue
  "Create an empty persistent queue or a persistent queue from a sequence."
  ([]    clojure.lang.PersistentQueue/EMPTY)
  ([seq] (into (queue) seq)))

(defmacro defm
  "Define a function with memoization. Takes the same arguments as defn."
  [& defn-args]
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

(defn syntax-quote ;; from leiningen.core/unquote-project
  "Syntax quote the given form, wrapping all seqs and symbols in quote."
  [form]
  (walk (fn [form]
          (cond (and (seq? form) (= `unquote (first form))) (second form)
                (or (seq? form) (symbol? form)) (list 'quote form)
                :else (syntax-quote form)))
        identity
        form))

(defmacro map-entry
  "Create a clojure.lang.MapEntry from a and b. Equivalent to a cons cell.
  useful.experimental.unicode contains a shortcut to this, named ·."
  [a b]
  `(clojure.lang.MapEntry. ~a ~b))

(defn pair
  "Create a clojure.lang.MapEntry from a and b. Equivalent to a cons cell"
  [a b]
  (map-entry a b))

(defn trade!
  "Like swap!, except it returns the old value of the atom."
  [atom f & args]
  (with-local-vars [prev nil]
    (apply swap! atom
           (fn [val & args]
             (var-set prev val)
             (apply f val args))
           args)
    (var-get prev)))
