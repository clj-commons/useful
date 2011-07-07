(ns useful.experimental.unicode
  (:use [useful.utils :only [map-entry]]
        [useful.macro :only [defalias macro-do]]))

(macro-do [dest src]
  `(defalias ~dest ~src)
  · map-entry)
