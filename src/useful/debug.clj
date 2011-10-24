(ns useful.debug)

;; leave out of ns decl so we can load with classlojure.io/resource-forms
(require 'clojure.stacktrace)
(require 'clojure.pprint)

(letfn [(interrogate-form [list-head form]
          `(let [display# (fn [val#]
                            (~@list-head (let [pretty# (with-out-str
                                                         (clojure.pprint/pprint ['~form '~'is val#]))]
                                           (subs pretty# 1 (- (count pretty#) 2)))))]
             (try (doto ~form display#)
                  (catch Throwable t#
                    (display# {:thrown t#
                               :trace (with-out-str
                                        (clojure.stacktrace/print-cause-trace t#))})
                    (throw t#)))))]

  (defmacro ?
    "A useful debugging tool when you can't figure out what's going on:
  wrap a form with ?, and the form will be printed alongside
  its result. The result will still be passed along."
    [val]
    (interrogate-form `(println) val))

  (defmacro ^{:dont-test "Complicated to test, and should work if ? does"}
    ?!
    ([val] `(?! "/tmp/spit" ~val))
    ([file val]
       (interrogate-form `(#(spit ~file (str % "\n") :append true)) val))))
