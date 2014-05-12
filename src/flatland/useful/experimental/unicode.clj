(ns ^{:dont-test "Just aliases for other functions/macros"}
  flatland.useful.experimental.unicode
  (:use [flatland.useful.utils :only [map-entry]]
        [flatland.useful.macro :only [macro-do]]
        [flatland.useful.ns :only [defalias]]))

(macro-do [dest src]
  `(defalias ~dest ~src)
  ∮ map-entry
  ! complement
  ∘ comp
  φ partial)
