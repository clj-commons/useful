(ns useful.test
  (:use [clojure.walk :only [postwalk]]))

(defmacro with-test-tags [tags & body]
  (let [tags (set (map keyword tags))]
    (cons `do
          (postwalk (fn [form]
                      (if (and (seq? form)
                               (symbol? (first form))
                               (= "deftest" (name (first form))))
                        (seq (update-in (vec form) [1]
                                        vary-meta update-in [:tags] (fnil into #{}) tags))
                        form))
                    body))))
