(ns useful.debug
  (:use clojure.stacktrace))

(letfn [(interrogate-form [list-head form]
          `(let [display# (fn [val#]
                            (~@list-head (prn-str '~form '~'is val#)))]
             (try (doto ~form display#)
                  (catch Throwable t#
                    (display# {:thrown t#
                               :trace (with-out-str
                                        (print-cause-trace t#))})
                    (throw t#)))))]

  (defmacro ?
    "A useful debugging tool when you can't figure out what's going on:
  wrap a form with ?, and the form will be printed alongside
  its result. The result will still be passed along."
    [val]
    (interrogate-form `(print) val))

  (defmacro ^{:dont-test "Complicated to test, and should work if ? does"}
    ?!
    ([val] `(?! "/tmp/spit" ~val))
    ([file val]
       (interrogate-form `(spit ~file :append true) val))))
