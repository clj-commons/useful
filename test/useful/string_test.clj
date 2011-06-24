(ns useful.string-test
  (:use useful.string clojure.test))

(deftest to-camel
  (are [in out] (= out (camelize in))
       "the-string" "theString"
       "this-is-real" "thisIsReal"
       "untouched" "untouched"))

(deftest from-camel
  (are [in dashed underscored] (= [dashed underscored]
                                  ((juxt dasherize underscore) in))
       "setSize"          "set-size"         "set_size"
       "theURL"           "the-url"          "the_url"
       "ClassName"        "class-name"       "class_name"
       "LOUD_CONSTANT"    "loud-constant"    "loud_constant"
       "the_CRAZY_train"  "the-crazy-train"  "the_crazy_train"
       "with-dashes"      "with-dashes"      "with_dashes"
       "with_underscores" "with-underscores" "with_underscores"))
