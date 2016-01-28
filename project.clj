(defproject org.flatland/useful "0.11.4"
  :description "A collection of generally-useful Clojure utility functions"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/amalloy/useful"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.macro "0.1.1"]
                 [org.clojure/tools.reader "0.7.2"]]
  :aliases {"testall" ["with-profile" "dev,default:dev,1.3,default:dev,1.4,default:dev,1.5,default:dev,1.7,default" "test"]}
  :profiles {:1.7 {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]} }
  :plugins [[codox "0.8.0"]]
  :codox {:src-dir-uri "http://github.com/flatland/useful/blob/develop/"
          :src-linenum-anchor-prefix "L"})
