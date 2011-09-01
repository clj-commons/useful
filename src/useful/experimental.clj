(ns useful.experimental
  (:use [useful.utils :only [split-vec]]
        [useful.seq :only [alternates]]
        [useful.map :only [keyed]]
        [useful.macro :only [name-with-attributes]]
        [useful.fn :only [any]]))

(defn comp-partial
  "A version of comp that \"rescues\" the first N args, passing them to every composed function
  instead of just the first one.

  For example, ((comp-partial 2 * +) 3 4 5 6) is equivalent to (* 3 4 (+ 3 4 5 6))."
  [n & fns]
  (let [split (if (neg? n)
                #(split-vec (vec %) n)
                #(split-at n %))]
    (fn [& args]
      (let [[rescued more] (split n args)
            fns (for [f fns] (apply partial f rescued))]
        (apply (apply comp fns) more)))))

(defmacro while-let
  "Repeatedly executes body, which presumably has side-effects, while let binding is not false."
  [bindings & body]
  (let [[form test] bindings]
    `(loop [~form ~test]
       (when ~form
         ~@body
         (recur ~test)))))

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

(defmacro order-let-if
  "If predicate is true, bind the names provided, otherwise reverse those bindings.

   Example:
    (order-let-if false [a \"foo\" b \"bar\"] [a b]) = [\"bar\" \"foo\"]"
  [pred bindings & body]
  `(if ~pred
     (let ~bindings ~@body)
     (let
         ~(vec
           (let [parts (partition 2 bindings)]
             (mapcat #(vector % (second %2))
                     (reverse (map first parts))
                     parts)))
       ~@body)))

(letfn [(mapify [coll] (into {} coll)) ;; just for less-deep indenting
        (symbol ([ns sym]              ;; annoying that (symbol 'x 'y) fails
                   (clojure.core/symbol (name ns) (name sym))))
        (behavior ([name default exceptions]
                     (= :forward
                        (if (exceptions name)
                          ({:forward :stub, :stub :forward} default)
                          default))))
        (analyze-var [v]
          (let [{:keys [ns name]} (meta v)
                ns (ns-name ns)
                sigs (:sigs @v)]
            (keyed [ns name sigs])))
        (append-if [test item coll]
          (if-not test
            coll
            (concat coll [item])))]

  (defmacro protocol-stub
    "Define a new type of record implementing the specified protocols. Its
  constructor will take two arguments:
    - An object which already satisfies the given protocols. This object will
      be delegated to for functions which are not stubbed out.
    - A \"log\" function to be called (for side effects) every time a protocol
      function is called. For functions marked as :stub (see below), the
      log function will be called with two arguments: the function name (an
      unqualified symbol), and the arglist (including \"this\"). Functions
      marked :forward will have a third argument, the function's return value.
      Use this function to implement your logging (or whatever else).

  The macro itself needs two arguments: the name of the record to define, and:
    - A map of protocol stubbing specifications. Each key should be a protocol,
      and the value another map. It may have zero or more of these keys:
      - A :default key specifying either :stub or :forward, to control whether
        the underlying implementation is called after logging. Defaults to :stub,
        meaning that only the logging function will be called, completely
        stubbing out the backing implementation.
      - An :exceptions key, listing the functions of this protocol that should
        behave the opposite of the :default."
    [name proto-specs]
    (let [[trace-field impl-field ret] (map gensym '(trace impl ret))
          [impl-kw trace-kw] (map keyword [impl-field trace-field])
          trace (fn [this] `(~trace-kw ~this))

          proto-fns
          (mapify
           (for [[name opts] proto-specs
                 :let [default-behavior (:default opts :stub)
                       exceptions (set (:exceptions opts))
                       proto-var (resolve name)
                       {:keys [ns name sigs]} (analyze-var proto-var)]]
             {(symbol ns name)
              (mapify
               (for [[fn-key {arglists :arglists, short-name :name}] sigs
                     :let [forward? (behavior short-name default-behavior exceptions)
                           fn-name (symbol ns short-name)]]
                 {fn-key
                  (cons `fn
                        (for [[this & args :as argvec] arglists
                              :let [proxy-args `((~impl-kw ~this) ~@args)]]
                          `([~@argvec]
                              (let [~ret ~(when forward?
                                            `(~fn-name ~@proxy-args))]
                                ~(->> `(~(trace this) '~short-name (list ~@proxy-args))
                                      (append-if forward? ret))
                                ~ret))))}))}))]
      `(do
         (defrecord ~name [~impl-field ~trace-field])
         (extend ~name
           ~@(apply concat proto-fns))))))

(defn wrap-with ^{:dont-test "Tested by make-wrappable!, wrap-multiple"}
  [f wrapper-var & [name]]
  (with-meta
    (fn [& args]
      (let [wrappers (not-empty @wrapper-var)]
        (if-not wrappers
          (apply f args)
          (with-bindings {wrapper-var
                          (vary-meta wrappers assoc
                                     ::call-data {:fn-name name})}
            (apply (reduce (fn [f wrapper]
                             (wrapper f))
                           f
                           wrappers)
                   args)))))
    (meta f)))

(defn make-wrappable! [fn-var wrappers-var & [name]]
  (alter-var-root fn-var wrap-with wrappers-var name))

(defmacro wrap-multiple [wrappers-var & fn-syms]
  (cons `do
        (for [f fn-syms]
          `(make-wrappable! #'~f ~wrappers-var '~f))))

(defmacro defn-wrapping
  "Define a function as with defn, which checks the contents of wrappers-var
  whenever it is called. If that var is empty, the underlying defn is called
  without modification. Otherwise, it is treated as a list of wrapper functions,
  which are wrapped around the underlying implementation before it is called.

  The wrappers are applied left-to-right, which means that the rightmost
  wrapper is outermost, and the leftmost wrapper is applied just before the base
  function.

  The wrappers are not called \"directly\" on the arguments, but are
  instead called like Ring wrappers, to create a single function composed of
  all of them; the resulting function is called with the actual arguments to
  the defn-wrapping function.

  For example, if the wrapped function is -, and the wrappers are
  [(fn [f] (fn [x] (* 2 (f x)))), (fn [f] (fn [x] (f (+ 10 x))))],
  then the eventual function will behave like (fn [x] (* 2 (- (+ 10 x)))).

  Swapping the order of the wrappers would yield a function behaving like
  (fn [x] (* 2 (+ 10 (- x)))).

  Note the order of the wrapping: when called with 10 as an argument, the former
  will return -40, and the latter 0."
  [name wrappers-var & defn-args]
  (let [[name macro-args] (name-with-attributes name defn-args)]
    `(doto (defn ~name ~@macro-args)
       (make-wrappable! ~wrappers-var '~name))))

(defmacro with-wrappers
  "Dynamically bind some additional wrappers to the specified wrapper-var
  (see defn-wrapping). Each wrapper function will be conj-ed onto the current
  set of wrappers."
  [wrappers-var wrap-fns & body]
  `(with-bindings {~wrappers-var (into @~wrappers-var ~wrap-fns)}
     ~@body))

(defmacro with-wrapper
  "Dynamically bind an additional wrapper to the specified wrapper-var
  (see defn-wrapping). The wrapper function will be conj-ed onto the current
  set of wrappers."
  [wrappers-var wrap-fn & body]
  `(with-wrappers ~wrappers-var [~wrap-fn] ~@body))

