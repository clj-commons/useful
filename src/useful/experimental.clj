(ns useful.experimental
  (:use [useful.utils :only [split-vec]]
        [useful.seq :only [alternates]]
        [useful.map :only [keyed]]
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

(comment
  (record-stub StubLayer
               (fn pr-stub
                 ([fn-name this args]
                    (println fn-name "called with" (pr-str args)))
                 ([fn-name this args ret]
                    (pr-stub fn-name this args)
                    (println "==> return" ret)))
               {Layer {:default :forward
                       :exceptions #{append-node!}}
                Append {:default :stub}})
  ;; expand to
  (defrecord StubLayer [impl])
  (let [print-stub (fn pr-stub
                     ([fn-name args]
                        (println fn-name "called with" (pr-str args)))
                     ([fn-name args ret]
                        (pr-stub fn-name args)
                        (println "==> return" ret)))]
    (extend StubLayer
      Layer
      {:append-node! (fn [this node whatever]
                       (print-stub "append-node!" this (list node whatever)))
       :open (fn [this]
               (print-stub "open" this (list) (jiraph.layer/open (:impl this))))})))


(letfn [(symbol ([ns sym]
                   (clojure.core/symbol (name ns) (name sym))))
        (behavior ([name default exceptions]
                     (if (exceptions name)
                       ({:forward :stub, :stub :forward} default)
                       default)))
        (analyze-var [v]
          (let [{:keys [ns name]} (meta v)
                ns (ns-name ns)
                sigs (:sigs @v)]
            (keyed [ns name sigs])))]
  (defmacro protocol-stub
    [name trace-fn proto-specs]
    (let [trace-fn-name (gensym 'trace-fn)
          impl-field-name (gensym 'impl)
          proto-fns (into {}
                          (for [[name opts] proto-specs
                                :let [default-behavior (:default opts :stub)
                                      exceptions (set (:exceptions opts))
                                      proto-var (resolve name)
                                      {:keys [ns name sigs]} (analyze-var proto-var)]]
                            {(symbol ns name)
                             (into {}
                                   (for [[fn-key {:keys [arglists name]}] sigs
                                         :let [behave (behavior name default-behavior exceptions)
                                               should-forward (= :forward behave)
                                               impl-sym (symbol ns name)]]
                                     {fn-key
                                      (cons `fn
                                            (for [[this & args :as sig] arglists]
                                              `(~sig (let [~'ret ~(when should-forward
                                                                    (list* impl-sym `(~(keyword impl-field-name) ~this) args))]
                                                       (~trace-fn-name
                                                        '~name
                                                        (list ~@sig)
                                                        ~@(when should-forward
                                                            ['ret]))
                                                       ~'ret))))}))}))]
      `(do
         (defrecord ~name [~impl-field-name])
         (let [~trace-fn-name ~trace-fn]
           (extend ~name
             ~@(apply concat proto-fns)))))))
