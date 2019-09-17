(defproject org.flatland/useful "0.11.7-SNAPSHOT"
  :description "A collection of generally-useful Clojure utility functions"
  :license {:name         "Eclipse Public License - v 1.0"
            :url          "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/amalloy/useful"
  :dependencies [[org.clojure/clojure "1.8.0"  :scope "provided"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/tools.reader "1.3.2"]]
  :aliases {"testall" ["with-profile" ":1.8:1.9:1.10" "test"]}

  :profiles {:dev      {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :1.10     {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :1.9      {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.8      {:dependencies [[org.clojure/clojure "1.8.0"]]}}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :plugins [[codox "0.8.0"]
            [lein-ancient "0.6.15"]]

  :codox {:src-dir-uri               "http://github.com/flatland/useful/blob/develop/"
          :src-linenum-anchor-prefix "L"})
