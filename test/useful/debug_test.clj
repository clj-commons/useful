(ns useful.debug-test
  (use useful.debug clojure.test))

(defmacro test-? [form]
  `(let [form# '~form
         expected# ~form
         collector# (java.io.StringWriter.)]
     (binding [*out* collector#]
       (is (= expected# (? ~form)))
       (let [written# (str collector#)]
         (are [val#] (.contains written# (pr-str val#))
              form# expected#)))))

(deftest ?-test ;; macro to avoid repeating expr with various levels of quoting
  (test-? (str "test" "more")))