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

;; copied from clojure.contrib.def
(defmacro ^{:dont-test "Exists in contrib, and has gross side effects anyway"}
  defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  ([name orig]
     `(do
        (alter-meta!
         (if (.hasRoot (var ~orig))
           (def ~name (.getRoot (var ~orig)))
           (def ~name))
         ;; When copying metadata, disregard {:macro false}.
         ;; Workaround for http://www.assembla.com/spaces/clojure/tickets/273
         #(conj (dissoc % :macro)
                (apply dissoc (meta (var ~orig)) (remove #{:macro} (keys %)))))
        (var ~name)))
  ([name orig doc]
     (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig)))

;; name-with-attributes by Konrad Hinsen, stolen from c.c.def:
(defn ^{:dont-test "Stolen from contrib"} name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args]          (if (map? (first macro-args))
                                     [(first macro-args) (next macro-args)]
                                     [{} macro-args])
        attr                       (if docstring
                                     (assoc attr :doc docstring)
                                     attr)
        attr                       (if (meta name)
                                     (conj (meta name) attr)
                                     attr)]
    [(with-meta name attr) macro-args]))

(defmacro with-altered-var
  "Binds var-name to the result of (f current-value args) for the dynamic
  scope of body. Basically like swap! or alter, but for vars."
  [[var-name f & args] & body]
  `(binding [~var-name (~f ~var-name ~@args)]
     ~@body))
