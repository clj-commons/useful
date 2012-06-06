(defproject useful "0.8.3-alpha2"
  :description "A collection of generally-useful Clojure utility functions"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.macro "0.1.1"]]
  :multi-deps {"1.3" [[org.clojure/clojure "1.3.0"]]
               "1.2" [[org.clojure/clojure "1.2.1"]]
               :all [[org.clojure/tools.macro "0.1.1"]]})
