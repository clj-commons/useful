(ns flatland.useful.string-test
  (:use flatland.useful.string clojure.test))

(deftest to-camel
  (are [in out] (= out (camelize in))
       "the-string" "theString"
       "this-is-real" "thisIsReal"
       "untouched" "untouched"))

(deftest to-class
  (are [in out] (= out (classify in))
       "the-string" "TheString"
       "this-is-real" "ThisIsReal"
       "touched" "Touched"))

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

(deftest pluralize-test
  (is (= "10 dogs" (pluralize 10 "dog")))
  (is (= "1 cat" (pluralize 1 "cat")))
  (is (= "0 octopodes" (pluralize 0 "octopus" "octopodes")))
  (is (= "1 fish" (pluralize 1 "fish" "fishes"))))

(deftest substring-after-test
  (let [s "foo:bar:baz-10"]
    (is (= "baz-10" ((substring-after ":") s)))
    (is (= "10" ((substring-after "-") s)))
    (is (= s ((substring-after "Q") s)))
    (is (= "z-10" ((substring-after "ba") s)))
    (is (= "" ((substring-after "0") s)))))
