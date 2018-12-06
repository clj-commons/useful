(defproject org.flatland/useful "0.11.5"
  :description "A collection of generally-useful Clojure utility functions"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/amalloy/useful"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.macro "0.1.1"]
                 [org.clojure/tools.reader "0.7.2"]]
  :aliases {"testall" ["with-profile" "+1.6:+1.7:+1.8:+1.9:+1.10" "test"]}
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.10.0-alpha3"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.0-RC3"]]}
             :1.9  {}
             :1.8  {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.7  {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.6  {:dependencies [[org.clojure/clojure "1.6.0"]]}}
  :plugins [[codox "0.8.0"]]
  :codox {:src-dir-uri "http://github.com/flatland/useful/blob/develop/"
          :src-linenum-anchor-prefix "L"})
