(ns ^{:dont-test "Just aliases for other functions/macros"}
  useful.experimental.unicode
  (:use [useful.utils :only [map-entry]]
        [useful.macro :only [macro-do]]
        [useful.alias :only [defalias]]))

(macro-do [dest src]
  `(defalias ~dest ~src)
  ∮ map-entry
  ! complement
  ∘ comp
  φ partial)
