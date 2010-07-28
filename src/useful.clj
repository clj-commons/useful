(ns useful)

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

(defn conj-vec
  "Conj onto collection ensuring it is a vector."
  [coll item]
  (conj (vec coll) item))

(defn into-vec
  "Returns a new vector consisting of to-coll with all of the items of from-coll conjoined."
  [to-coll from-coll]
  (into (vec to-coll) from-coll))

(defn include?
  "Check if val exists in coll."
  [val coll]
  (some (partial = val) coll))

(defmacro if-ns [ns-reference then-form else-form]
  "Try to load a namespace reference. If sucessful, evaluate then-form otherwise evaluate else-form."
  `(try (ns ~(.getName *ns*) ~ns-reference)
        (eval '~then-form)
        (catch java.io.FileNotFoundException e#
          (eval '~else-form))))

(defn tap
  "Call f on obj, presumably with side effects, then return obj. Useful for debugging when
   you want to print an object inline. e.g. (tap println foo)"
  [f obj]
  (f obj)
  obj)

(defn update
  "Update value in map where f is a function that takes the old value and the
   supplied args and returns the new value."
  [map key f & args]
  (assoc map key (apply f (get map key) args)))

(defmacro while-let
  "Repeatedly executes body while let binding is true."
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

(defmacro verify
  "Execute body if test returns true, otherwise raise exception."
  [test exception & body]
  `(if ~test
     (do ~@body)
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
     (alter-var-root var (fn [f#] with-meta (memoize f#) (meta f#)))
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

(defn invoke-private
  "Invoke a private or protected Java method. Be very careful when using this!
   I take no responsibility for the trouble you get yourself into."
  [method instance & params]
  (let [signature (into-array Class (map class params))]
    (when-let [method (first (remove nil? (for [c (ancestors (.getClass instance))]
                                            (try (.getDeclaredMethod c method signature)
                                                 (catch NoSuchMethodException e)))))]
      (let [accessible (.isAccessible method)]
        (.setAccessible method true)
        (let [result (.invoke method instance (into-array params))]
          (.setAccessible method false)
          result)))))

(defn- parse-opt [default opts arg]
  (let [m re-matches, key (comp keyword str)]
    (cond-let
     [[_ ks]  (m #"-(\w*)"                arg)] (apply merge-with into-vec opts (for [k ks] {(key k) [""]}))
     [[_ k v] (m #"--?([-\w]*)=([-,\S]+)" arg)] (update opts (key k) into-vec (.split #"," v))
     [[_ k]   (m #"--?([-\w]*)"           arg)] (update opts (key k) conj-vec "")
     :else                                      (update opts default conj-vec arg))))

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