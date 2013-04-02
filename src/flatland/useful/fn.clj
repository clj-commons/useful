(ns flatland.useful.fn)

(def ! complement)

(defn validator
  "Create a version of a predicate that only tests its output for truthiness,
  returning the original input value if the predicate evaluates to anything
  truthy, and nil otherwise. ((validator even?) 10) => 10, even though
  (even? 10) is true."
  [pred]
  (fn [x]
    (when (pred x)
      x)))

(defn decorate
  "Return a function f such that (f x) => [x (f1 x) (f2 x) ...]."
  [& fs]
  (apply juxt identity fs))

(defn annotate
  "A vector of [x (f1 x) (f2 x) ...]."
  [x & fs]
  ((apply decorate fs) x))

(defn as-fn
  "Turn an object into a fn if it is not already, by wrapping it in constantly."
  [x]
  (if (ifn? x) x, (constantly x)))

(defn fix
  "Walk through clauses, a series of predicate/transform pairs. The
  first predicate that x satisfies has its transformation clause
  called on x. Predicates or transforms may be values (eg true or nil)
  rather than functions; these will be treated as functions that
  return that value.

  The last \"pair\" may be only a transform with no pred: in that case it
  is unconditionally used to transform x, if nothing previously matched.

  If no predicate matches, then x is returned unchanged."
  [x & clauses]
  (let [call #((as-fn %) x)]
    (first (or (seq (for [[pred & [transform :as exists?]] (partition-all 2 clauses)
                          :let [[pred transform] ;; handle odd number of clauses
                                (if exists? [pred transform] [true pred])]
                          :when (call pred)]
                      (call transform)))
               [x]))))

(defn to-fix
  "A \"curried\" version of fix, which sets the clauses once, yielding a
  function that calls fix with the specified first argument."
  [& clauses]
  (fn [x]
    (apply fix x clauses)))

(defn fixing
  "A version of fix that fits better with the unified update model: instead of multiple clauses,
   additional args to the transform function are permitted. For example,
   (swap! my-atom fixing map? update-in [k] inc)"
  [x pred transform & args]
  (if ((as-fn pred) x)
    (apply (as-fn transform) x args)
    x))

(defmacro given
  "A macro combining the features of fix and fixing, by using parentheses to group the
   additional arguments to each clause:
   (-> x
       (given string? read-string
              map? (dissoc :x :y :z)
              even? (/ 2)))"
  [x & clauses]
  (let [[clauses default] (if (even? (count clauses))
                            [clauses `identity]
                            [(butlast clauses) (last clauses)])]
    `(fix ~x ~@(for [[pred transform] (partition 2 clauses)
                     arg [pred `#(-> % ~transform)]]
                 arg)
          ~default)))

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

(defn knit
  "Takes a list of functions (f1 f2 ... fn) and returns a new function F. F takes
   a collection of size n (x1 x2 ... xn) and returns a vector [(f1 x1) (f2 x2) ... (fn xn)].
   Similar to Haskell's ***, and a nice complement to juxt (which is Haskell's &&&)."
  [& fs]
  (fn [arg-coll]
    (vec (map #(% %2) fs arg-coll))))

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
    ([] (f))
    ([a] (if (nil? a)
           (f)
           (f a)))
    ([a b]
       (if (nil? a)
         (if (nil? b)
           (f)
           (f b))
         (if (nil? b)
           (f a)
           (f a b))))
    ([a b & more]
       (when-let [items (seq (remove nil? (list* a b more)))]
         (apply f items)))))

(defn key-comparator
  "Given a transformation function (and optionally a direction), return a
  comparator which does its work by comparing the values of (transform x) and
  (transform y)."
  ([modifier]
     (fn [a b]
       (compare (modifier a) (modifier b))))
  ([direction modifier]
     (let [f (key-comparator modifier)]
       (condp #(% %2) direction
         #{:desc :descending -} (comp - f)
         #{:asc :ascending +} f))))

(defn rate-limited
  "Create a version of a function which 'refuses' to be called too
  frequently. If it has successfully been called in the last N milliseconds,
  calls to it will return nil; if no calls have succeeded in that period, args
  will be passed along to the base function."
  [f ms-period]
  (let [tracker (atom {:last-sent 0})]
    (fn [& args]
      (when (:accepted (swap! tracker
                              (fn [{:keys [last-sent]}]
                                (let [now (System/currentTimeMillis)
                                      ok (< ms-period (- now last-sent))]
                                  {:accepted ok
                                   :last-sent (if ok now last-sent)}))))
        (apply f args)))))

(defn memoize-last
  "A version of memoize that only remembers the result for a single input
   argument at a time. eg, if you call (f 1) (f 1) (f 2) (f 1), only the
   second call is memoized, because it is the same argument you just gave.
   The third and fourth calls see a new argument, and therefore refresh the
   cached value."
  [f]
  (let [cache (atom nil)]
    (fn [& args]
      (:value (swap! cache
                     (fn [cache]
                       (if (= args (get cache :args ::not-found))
                         cache
                         {:args args, :value (apply f args)})))))))
