(ns flatland.useful.utils
  (:use [clojure.walk :only [walk]]
        [flatland.useful.fn :only [decorate ignoring-nils fix]]
        [clojure.tools.macro :only [symbol-macrolet]])
  (:import (clojure.lang IDeref ISeq IPersistentMap IPersistentSet IPersistentCollection)))

(defn invoke
  "Like clojure.core/apply, but doesn't expand/splice the last argument."
  ([f] (f))
  ([f x] (f x))
  ([f x & more] (apply f x more)))

(defmacro verify
  "Raise exception unless test returns true."
  ([test exception]
     `(when-not ~test
        (throw (fix ~exception string? #(Exception. ^String %)))))
  ([test msg info]
     `(when-not ~test
        (throw (ex-info ~msg ~info)))))

(defmacro returning
  "Compute a return value, then execute other forms for side effects.
  Like prog1 in common lisp, or a (do) that returns the first form."
  [value & forms]
  `(let [value# ~value]
     ~@forms
     value#))

(letfn [(no-arg-nil [f]
          (fn
            ([] nil)
            ([& args] (apply f args))))]

  (def ^{:doc "The minimium value of vals, ignoring nils."
         :arglists '([& args])}
    or-min (ignoring-nils (no-arg-nil min)))

  (def ^{:doc "The maximium value of vals, ignoring nils."
         :arglists '([& args])}
    or-max (ignoring-nils (no-arg-nil max))))

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

(defn into-set
  "Update the given set using an existence map."
  [set map]
  (if (map? map)
    (reduce (fn [set [k v]] ((if v conj disj) set k))
            set map)
    (into set map)))

(defprotocol Adjoin
  (adjoin-onto [left right]
    "Merge two data structures by combining the contents. For maps, merge recursively by
  adjoining values with the same key. For collections, combine the right and left using
  into or conj. If the left value is a set and the right value is a map, the right value
  is assumed to be an existence map where the value determines whether the key is in the
  merged set. This makes sets unique from other collections because items can be deleted
  from them."))

(extend-protocol Adjoin
  IPersistentMap
  (adjoin-onto [this other]
    (merge-with adjoin-onto this other))

  IPersistentSet
  (adjoin-onto [this other]
    (into-set this other))

  ISeq
  (adjoin-onto [this other]
    (concat this other))

  IPersistentCollection
  (adjoin-onto [this other]
    (into this other))

  Object
  (adjoin-onto [this other]
    other)

  nil
  (adjoin-onto [this other]
    other))

(defn adjoin
  "Merge two data structures by combining the contents. For maps, merge recursively by
  adjoining values with the same key. For collections, combine the right and left using
  into or conj. If the left value is a set and the right value is a map, the right value
  is assumed to be an existence map where the value determines whether the key is in the
  merged set. This makes sets unique from other collections because items can be deleted
  from them."
  [a b]
  (adjoin-onto a b))

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

(defn update-peek
  "Update the element in stack that would be returned by peek, returning a new stack."
  [stack f & args]
  (conj (pop stack)
        (apply f (peek stack) args)))

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
  flatland.useful.experimental.unicode contains a shortcut to this, named ·."
  [a b]
  `(clojure.lang.MapEntry. ~a ~b))

(defn pair
  "Create a clojure.lang.MapEntry from a and b. Equivalent to a cons cell"
  [a b]
  (map-entry a b))

(defn ^{:dont-test "Used in impl of thread-local"}
  thread-local*
  "Non-macro version of thread-local - see documentation for same."
  [init]
  (let [generator (proxy [ThreadLocal] []
                    (initialValue [] (init)))]
    (reify IDeref
      (deref [this]
        (.get generator)))))

(defmacro thread-local
  "Takes a body of expressions, and returns a java.lang.ThreadLocal object.
   (see http://download.oracle.com/javase/6/docs/api/java/lang/ThreadLocal.html).

   To get the current value of the thread-local binding, you must deref (@) the
   thread-local object. The body of expressions will be executed once per thread
   and future derefs will be cached.

   Note that while nothing is preventing you from passing these objects around
   to other threads (once you deref the thread-local, the resulting object knows
   nothing about threads), you will of course lose some of the benefit of having
   thread-local objects."
  [& body]
  `(thread-local* (fn [] ~@body)))

(defn read-seq
  "Read all forms from *in* until an EOF is reached. Throws an exception on incomplete forms."
  []
  (lazy-seq
   (let [form (read *in* false ::EOF)]
     (when-not (= ::EOF form)
       (cons form (read-seq))))))

(defmacro let-later
  "Behaves like let, but bindings which have :delay metadata on them are
   evaluated lazily, by placing their values in a delay and forcing the
   delay whenever the body of the let-later needs the value. For example,

   (let-later [^:delay a (do-stuff)]
     (when (whatever) a))

   will only evaluate (do-stuff) if (whatever) is true."
  [bindings & body]
  (reduce (fn [body [name val]]
            (if (:delay (meta name))
              (if (symbol? name)
                (let [delay-sym (gensym (str "delay-" name))]
                  `(let [~delay-sym (delay ~val)]
                     (symbol-macrolet [~name (force ~delay-sym)]
                       ~body)))
                `(let-later [~@(apply concat
                                      (for [[k v] (partition 2 (destructure [name val]))]
                                        [(vary-meta k assoc :delay true)
                                         v]))]
                   ~body))
              `(let [~name ~val]
                 ~body)))
          `(do ~@body)
          (reverse (partition 2 bindings))))

(defn copy-meta
  "Copy all the metadata from src to dest."
  [dest src]
  (with-meta dest (meta src)))

(defn empty-coll?
  "Is x a collection and also empty?"
  [x]
  (or (nil? x)
      (and (coll? x)
           (empty? x))))

(defmacro switch
  "Like case, but uses object equality instead of the compile-time hash. Also, switch does not
  require a default clause. Of course, switch is not as efficient as case, but it works for things
  like functions, which case cannot support."
  [expr & clauses]
  (let [[clauses default] (if (even? (count clauses))
                            [clauses nil]
                            [(butlast clauses) (last clauses)])]
    `(condp contains? ~expr
       ~@(mapcat (fn [[val form]]
                   [(fix val, list? set, hash-set) form])
                 (partition 2 clauses))
       ~default)))

(defmacro with-timing
  "Same as clojure.core/time but returns a vector of a the result of
   the code and the milliseconds rather than printing a string. Runs
   the code in an implicit do."
  [& body]
  `(let [start# (System/nanoTime)
         ret# ~(cons 'do body)]
     [ret# (/ (double (- (System/nanoTime) start#)) 1000000.0)]))