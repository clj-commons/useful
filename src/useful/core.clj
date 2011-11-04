(ns useful.core
  (:use [useful.alias :only [alias-ns]]))

(doseq [module '(bean cli compress datatypes debug dispatch experimental experimental.unicode fn java macro map parallel seq state string test utils)]
  (alias-ns (symbol (str "useful." module))))
