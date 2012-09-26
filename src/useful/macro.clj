(ns useful.macro
  (:use [clojure.tools.macro :only [macrolet]]))

(defmacro anon-macro
  "Define, and then immediately use, an anonymous macro. For
example, (anon-macro [x y] `(def ~x ~y) myconst 10) expands to (def
myconst 10)."
  ([args macro-body & body]
     `(macrolet [(name# ~args ~macro-body)]
        (name# ~@body))))

(letfn [(partition-params [argvec actual-args]
          (if (some #{'&} argvec)
            [actual-args]               ; one seq with all args
            (vec (map vec (partition (count argvec) actual-args)))))]

  (defmacro macro-do
    "Wrap a list of forms with an anonymous macro, which partitions the
   forms into chunks of the right size for the macro's arglists. The
   macro's body will be called once for every N items in the args
   list, where N is the number of arguments the macro accepts. The
   result of all expansions will be glued together in a (do ...) form.

   Really, the macro is only called once, and is adjusted to expand
   into a (do ...) form, but this is probably an implementation detail
   that I'm not sure how a client could detect.

   For example,
   (macro-do [[f & args]]
             `(def ~(symbol (str \"basic-\" f))
                (partial ~f ~@args))
             [f 'test] [y 1 2 3])
   expands into (do
                 (def basic-f (partial f 'test))
                 (def basic-y (partial y 1 2 3)))"
    ([macro-args body & args]
       `(anon-macro [arg#]
                    (cons 'do
                          (for [~macro-args arg#]
                            ~body))
                    ~(partition-params macro-args args)))))
