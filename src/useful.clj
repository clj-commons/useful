(ns useful
  (:use [clojure.walk :only [walk]]))

(defmacro assoc-if
  "Create mapping from keys to values in map if test returns true."
  [map test & kvs]
  (let [assoc (cons 'assoc (cons map kvs))]
    `(if ~test
       ~assoc
       ~map)))

(defn assoc-or
  "Create mapping from each key to val in map only if existing val is nil."
  ([map key val]
     (if (nil? (map key))
       (assoc map key val)
       map))
  ([map key val & kvs]
     (let [map (assoc-or map key val)]
       (if kvs
         (recur map (first kvs) (second kvs) (nnext kvs))
         map))))

(defn or-min
  "The minimium value of vals, ignoring nils."
  [& vals]
  (when-let [vals (seq (remove nil? vals))]
    (apply min vals)))

(defn or-max
  "The maximum value of vals, ignoring nils."
  [& vals]
  (when-let [vals (seq (remove nil? vals))]
    (apply max vals)))

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

(defn into-map
  "Convert a list of heterogeneous args into a map. Args can be alternating keys and values,
   maps of keys to values or collections of alternating keys and values."
  [& args]
  (loop [args args map {}]
    (if (empty? args)
      map
      (let [arg  (first args)
            args (rest args)]
       (cond
         (nil?  arg) (recur args map)
         (map?  arg) (recur args (merge map arg))
         (coll? arg) (recur (into args (reverse arg)) map)
         :else       (recur (rest args) (assoc map arg (first args))))))))

(defn map-vals
  "Create a new map from m by calling function f on each value to get a new value."
  [f m]
  (into {}
        (for [[k v] m]
          [k (f v)])))

(defn map-vals-with-keys
  "Create a new map from m by calling function f on each key and value to get a new value."
  [f m]
  (into {}
        (for [[k v] m]
          [k (f k v)])))

(defn map-reduce
  "Perform a map and a reduce over a collection in a single pass. Unlike map, this is not lazy.
   Returns the equivalent of [(vec (map map-fn coll)) (reduce reduce-fn val coll)]."
  ([map-fn reduce-fn reduce-val coll & [map-val]]
     (reduce
      (fn [results item]
        (let [item (map-fn item)]
          [(conj (first results) item)
           (reduce-fn (second results) item)]))
      [(into [] map-val) reduce-val]
      coll))
  ([map-fn reduce-fn coll]
     (let [val (map-fn (first coll))]
       (map-reduce map-fn reduce-fn val (rest coll) [val]))))

(defn include?
  "Check if val exists in coll."
  [val coll]
  (some (partial = val) coll))

(defn extract
  "Extracts the first item that matches pred from coll, returning a vector of that item
   followed by coll with the item removed."
  [pred coll]
  (let [[head [item & tail]] (split-with (complement pred) coll)]
    [item (concat head tail)]))

(defn separate
  "Split coll into two sequences, one that matches pred and one that doesn't. Unlike the
  version in clojure.contrib.seq-utils, pred is only called once per item."
  [pred coll]
  (let [pcoll (map #(vector % (pred %)) coll)]
    (vec (map #(map first (% second pcoll)) [filter remove]))))

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
  (f obj)
  obj)

(defn update
  "Update value in map where f is a function that takes the old value and the supplied args and
   returns the new value. For efficiency, Do not change map if the old value is the same as the new
   value. If key is sequential, update all keys in the sequence with the same function."
  [map key f & args]
  (if (sequential? key)
    (reduce #(apply update %1 %2 f args) map key)
    (let [old (get map key)
          new (apply f old args)]
      (if (= old new) map (assoc map key new)))))

(defn append
  "Merge two data structures by combining the contents. For maps, merge recursively by
  appending values with the same key. For collections, combine the right and left using
  into or conj. If the left value is a set and the right value is a map, the right value
  is assumed to be an existence map where the value determines whether the key is in the
  merged set. This makes sets unique from other collections because items can be deleted
  from them."
  [left right]
  (cond (map? left)
        (merge-with append left right)

        (and (set? left) (map? right))
        (reduce (fn [set [k v]] ((if v conj disj) set k))
                left right)

        (coll? left)
        ((if (coll? right) into conj) left right)

        :else right))

(defn merge-in
  "Merge two nested maps."
  [left right]
  (if (map? left)
    (merge-with merge-in left right)
    right))

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

(defn abort
  "Print message then exit."
  [& message]
  (apply println message)
  (System/exit 1))

(defmacro rescue
  "Evaluate form, returning error-form on any Exception."
  [form error-form]
  `(try ~form (catch Exception e# ~error-form)))

(defmacro verify
  "Raise exception unless test returns true."
  [test exception]
  `(when-not ~test
     (throw (if (string? ~exception)
              (Exception. ~exception)
              ~exception))))

(defn trap
  "Register signal handling function."
  [signal f]
  (sun.misc.Signal/handle
   (sun.misc.Signal. signal)
   (proxy [sun.misc.SignalHandler] []
     (handle [sig] (f sig)))))

(defmacro defm [name & fdecl]
  "Define a function with memoization. Takes the same arguments as defn."
  `(let [var (defn ~name ~@fdecl)]
     (alter-var-root var (fn [f#] (with-meta (memoize f#) (meta f#))))
     var))

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

(defn zip
  "Returns a lazy sequence of vectors of corresponding items from each collection. If one collection
   is longer than the others, the missing items will be filled in with nils."
  [& colls]
  (lazy-seq
   (when (some seq colls)
     (cons (vec (map first colls))
           (apply zip (map rest colls))))))

(defn find-with
  "Returns the val corresponding to the first key where (pred key) returns true."
  [pred keys vals]
  (last (first (filter (comp pred first) (map vector keys vals)))))

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

(defn slice
  "Divide coll into n approximately equal slices."
  [n coll]
  (loop [num n, slices [], items (vec coll)]
    (if (empty? items)
      slices
      (let [size (Math/ceil (/ (count items) num))]
        (recur (dec num) (conj slices (subvec items 0 size)) (subvec items size))))))

(def *pcollect-thread-num* (.. Runtime getRuntime availableProcessors))

(defn pcollect
  "Like pmap but not lazy and more efficient for less computationally intensive functions
   because there is less coordination overhead. The collection is sliced among the
   available processors and f is applied to each sub-collection in parallel using map."
  ([f coll]
     (pcollect identity f coll))
  ([wrap-fn f coll]
     (if (<= *pcollect-thread-num* 1)
       ((wrap-fn #(doall (map f coll))))
       (mapcat deref
               (map (fn [slice]
                      (let [body (wrap-fn #(doall (map f slice)))]
                        (future-call body)))
                    (slice *pcollect-thread-num* coll))))))

(defn wrap-bindings
  "Wrap f in a new fuction that re-establishes the current binding for the given vars."
  [vars f]
  (let [bindings (select-keys (get-thread-bindings) vars)]
    (fn [& args]
      (push-thread-bindings bindings)
      (try
        (apply f args)
        (finally
         (pop-thread-bindings))))))

(defn assoc-in!
  "Associates a value in a nested associative structure, where ks is a sequence of keys
  and v is the new value and returns a new nested structure. The associative structure
  can have transients in it, but if any levels do not exist, non-transient hash-maps will
  be created."
  [m [k & ks] v]
  (let [assoc (if (instance? clojure.lang.ITransientCollection m) assoc! assoc)]
    (if ks
      (assoc m k (assoc-in! (get m k) ks v))
      (assoc m k v))))

(defn update-in!
  "'Updates' a value in a nested associative structure, where ks is a sequence of keys and
  f is a function that will take the old value and any supplied args and return the new
  value, and returns a new nested structure. The associative structure can have transients
  in it, but if any levels do not exist, non-transient hash-maps will be created."
  [m [k & ks] f & args]
  (let [assoc (if (instance? clojure.lang.ITransientCollection m) assoc! assoc)]
    (if ks
      (assoc m k (apply update-in! (get m k) ks f args))
      (assoc m k (apply f (get m k) args)))))

(defn thrush
  "Takes the first argument and applies the remaining arguments to it as functions from left to right.
   This tiny implementation was written by Chris Houser. http://blog.fogus.me/2010/09/28/thrush-in-clojure-redux"
  [& args]
  (reduce #(%2 %1) args))

(defn comp-partial
  "Like comp, except all args but the last are passed to every function with the last arg threaded through
   these partial functions. So, the rightmost fn is applied to all arguments. Each fn is then applied to the
   original args with the last arg replaced by the result of the previous fn."
  [& fns]
  (fn [& args]
    (let [f (apply comp (map #(apply partial % (butlast args)) fns))]
      (f (last args)))))

(defn position
  "Returns a map from item to the position of its first occurance in coll."
  [coll]
  (into {} (reverse (map-indexed #(vector %2 %1) coll))))

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
   For example:
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

(defn construct
  "Construct a new instance of class using reflection."
  [class & args]
  (clojure.lang.Reflector/invokeConstructor class (into-array Object args)))

(defn invoke-private
  "Invoke a private or protected Java method. Be very careful when using this!
   I take no responsibility for the trouble you get yourself into."
  [instance method & params]
  (let [signature (into-array Class (map class params))]
    (when-let [method (first (remove nil? (for [c (ancestors (.getClass instance))]
                                            (try (.getDeclaredMethod c method signature)
                                                 (catch NoSuchMethodException e)))))]
      (let [accessible (.isAccessible method)]
        (.setAccessible method true)
        (let [result (.invoke method instance (into-array params))]
          (.setAccessible method false)
          result)))))

(defn on-shutdown
  "Execute the given function on jvm shutdown."
  [f]
  (.addShutdownHook
   (Runtime/getRuntime)
   (proxy [Thread] []
     (run [] (f)))))

(defn- parse-opt [default opts arg]
  (let [m re-matches, key (comp keyword str)]
    (cond-let
     [[_ ks]  (m #"-(\w+)"           arg)] (apply merge-with into-vec opts (for [k ks] {(key k) [""]}))
     [[_ k v] (m #"--?([-\w]+)=(.+)" arg)] (update opts (key k) into-vec (.split #"," v))
     [[_ k]   (m #"--?([-\w]+)"      arg)] (update opts (key k) conj-vec "")
     :else                                 (update opts default conj-vec arg))))

(defn parse-opts
  "Parse command line args or the provided argument list. Returns a map of keys to
   vectors of repeated values. Named args begin with --keyname and are mapped to
   :keyname. Unnamed arguments are mapped to nil or default. Repeated named values can be
   specified by repeating a key or by using commas in the value. Single and double dashes
   are both supported though a single dash followed by word characters without internal
   dashes or an equal sign is assumed to be single character argument flags and are split
   accordingly.

   Example:
     (parse-opts [\"foo\" \"-vD\" \"bar\" \"-no-wrap\" \"-color=blue,green\" \"--style=baroque\" \"-color=red\"])
     => {:style [\"baroque\"], :color [\"blue\" \"green\" \"red\"], :no-wrap [\"\"], :D [\"\"], :v [\"\"], nil [\"foo\" \"bar\"]}"
  ([] (parse-opts nil *command-line-args*))
  ([args] (parse-opts nil args))
  ([default args] (reduce (partial parse-opt default) {} args)))