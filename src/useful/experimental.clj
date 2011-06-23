(ns useful.experimental
  (:use [useful.utils :only [split-vec]]))

(defn comp-partial
  "A version of comp that \"rescues\" the first N args, passing them to every
composed function instead of just the first one.

For example, ((comp-partial 2 * +) 3 4 5 6) is equivalent to (* 3 4 (+ 3 4 5 6))."
  [n & fns]
  (let [split (if (neg? n)
                #(split-vec (vec %) n)
                #(split-at n %))]
    (fn [& args]
      (let [[rescued more] (split n args)
            fns (for [f fns] (apply partial f rescued))]
        (apply (apply comp fns) more)))))