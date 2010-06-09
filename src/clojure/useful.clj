(ns clojure.useful
  (:use [clojure.contrib.str-utils2 :only [split]]))

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

(defmacro defm [name & fdecl]
  "Define a function with memoization. Takes the same arguments as defn."
  `(let [var (defn ~name ~@fdecl)]
     (alter-var-root var #(with-meta (memoize %) (meta %)))
     var))

(defn into-vec
  "Returns a new vector consisting of to-coll with all of the items of from-coll conjoined."
  [to-coll from-coll]
  (into (vec to-coll) from-coll))

(defn include?
  "Check if val exists in coll."
  [val coll]
  (some (partial = val) coll))

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

(defn- parse-arg [default opts arg]
  (if-let [[_ k _ v] (re-matches #"--?([-\w]*)(=([-,\w]*))?" arg)]
    (update opts (keyword k) into-vec (split (or v "") #","))
    (update opts default conj-vec arg)))

(defn parse-args
  "Parse command line args or the provided argument list. Returns a map of keys to
   vectors of repeated values. Named args begin with -keyname and are mapped to
   :keyname. Unnamed arguments are mapped to nil or default. Repeated named values can also
   be specified using commas in the value. Single and double dash are both supported.

   Example:
     (parse-args [\"foo\" \"-v\" \"bar\" \"-color=blue,green\" \"--style=baroque\" \"-color=red\"])
     => {:style \"baroque\", :color [\"blue\" \"green\" \"red\"], :v [\"\"], nil [\"foo\" \"bar\"]}"
  ([] (parse-args nil *command-line-args*))
  ([args] (parse-args nil args))
  ([default args] (reduce (partial parse-arg default) {} args)))