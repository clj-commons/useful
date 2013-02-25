(defproject org.flatland/useful "0.9.4"
  :description "A collection of generally-useful Clojure utility functions"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/flatland/useful"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.macro "0.1.1"]]
  :aliases {"testall" ["with-profile" "dev,default:dev,1.3,default:dev,1.5,default" "test"]}
  :profiles {:1.5 {:dependencies [[org.clojure/clojure "1.5.0-RC16"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}})
