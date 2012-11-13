(ns flatland.useful.time
  (:import [java.util.concurrent TimeUnit]))

(def ^{:doc "Convert a Clojure keyword into a java.util.concurrent.TimeUnit"
       :attribution "I stole this from my Clojail implementation"}
  unit
  (into {} (for [[enum aliases] {TimeUnit/NANOSECONDS [:ns :nanoseconds]
                                 TimeUnit/MICROSECONDS [:us :microseconds]
                                 TimeUnit/MILLISECONDS [:ms :milliseconds]
                                 TimeUnit/SECONDS [:s :sec :seconds]
                                 TimeUnit/MINUTES [:m :min :minutes]
                                 TimeUnit/HOURS [:h :hr :hours]
                                 TimeUnit/DAYS [:d :day :days]}
                 alias aliases]
             {alias enum})))

(defn duration [num unit-keyword]
  {:num num, :unit (unit unit-keyword)})
