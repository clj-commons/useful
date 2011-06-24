(ns useful.cli
  (:use [useful.utils :only [cond-let into-vec conj-vec]]
        [useful.maps :only [update]]))

(defn- parse-opt [default opts arg]
  (let [m re-matches, key (comp keyword str)]
    (cond-let
     [[_ ks]  (m #"-(\w+)"           arg)] (apply merge-with into-vec opts (for [k ks] {(key k) [""]}))
     [[_ k v] (m #"--?([-\w]+)=(.+)" arg)] (update opts (key k) into-vec (.split #"," v))
     [[_ k]   (m #"--?([-\w]+)"      arg)] (update opts (key k) conj-vec "")
     :else                                 (update opts default conj-vec arg))))

(defn parse-opts
  "Parse command line args or the provided argument list. Returns a map of keys to
   vectors of repeated values. Named args begin with --keyname and are mapped to
   :keyname. Unnamed arguments are mapped to nil or default. Repeated named values can be
   specified by repeating a key or by using commas in the value. Single and double dashes
   are both supported though a single dash followed by word characters without internal
   dashes or an equal sign is assumed to be single character argument flags and are split
   accordingly.

   Example:
     (parse-opts [\"foo\" \"-vD\" \"bar\" \"-no-wrap\" \"-color=blue,green\" \"--style=baroque\" \"-color=red\"])
     => {:style [\"baroque\"], :color [\"blue\" \"green\" \"red\"], :no-wrap [\"\"], :D [\"\"], :v [\"\"], nil [\"foo\" \"bar\"]}"
  ([] (parse-opts nil *command-line-args*))
  ([args] (parse-opts nil args))
  ([default args] (reduce (partial parse-opt default) {} args)))