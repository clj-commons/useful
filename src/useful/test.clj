(ns useful.test
  (:use [clojure.tools.macro :only [macrolet]]))

(defmacro with-test-tags [tags & body]
  (let [tags (set (map keyword tags))
        deftest-decl
        (list 'deftest '[name & body]
              (list 'let ['n `(vary-meta ~'name update-in [:tags]
                                         (fnil into #{}) ~tags)
                          'form `(list* '~'clojure.test/deftest ~'n ~'body)]
                    'form))
        with-test-tags-decl
        (list 'with-test-tags '[tags & body]
              `(list* 'with-test-tags
                      (into ~tags (map keyword ~'tags)) ~'body))]
    `(macrolet [~deftest-decl
                ~with-test-tags-decl]
       ~@body)))
