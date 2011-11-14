(ns useful.core
  (:use [useful.ns :only [alias-ns]]))

(doseq [module '(bean cli compress datatypes debug dispatch experimental experimental.unicode fn io java macro map parallel seq state string test utils)]
  (alias-ns (symbol (str "useful." module))))
