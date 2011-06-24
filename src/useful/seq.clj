(ns useful.seq
  (:use [useful.amalloy :only [decorate]]))

(defn extract
  "Extracts the first item that matches pred from coll, returning a vector of that item
   followed by coll with the item removed."
  [pred coll]
  (let [[head [item & tail]] (split-with (complement pred) coll)]
    [item (concat head tail)]))

(defn separate
  "Split coll into two sequences, one that matches pred and one that doesn't. Unlike the
  version in clojure.contrib.seq-utils, pred is only called once per item."
  [pred coll]
  (let [pcoll (map (decorate pred) coll)]
    (vec (for [f [filter remove]]
           (map first (f second pcoll))))))

(defn zip
  "Returns a lazy sequence of vectors of corresponding items from each collection. If one collection
   is longer than the others, the missing items will be filled in with nils."
  [& colls]
  (lazy-seq
   (when (some seq colls)
     (cons (vec (map first colls))
           (apply zip (map rest colls))))))

(defn slice
  "Divide coll into n approximately equal slices."
  [n coll]
  (loop [num n, slices [], items (vec coll)]
    (if (empty? items)
      slices
      (let [size (Math/ceil (/ (count items) num))]
        (recur (dec num) (conj slices (subvec items 0 size)) (subvec items size))))))

(defn cross
  "Computes the cartesian-product of the provided seqs. In other words, compute the set of all
  possible combinations of ways you can choose one item from each seq."
  [& seqs]
  (if (seq (rest seqs))
    (for [x (first seqs)
          y (apply cross (rest seqs))]
      (cons x y))
    (map list (first seqs))))

(defn lazy-cross
  "Compute a lazy cartesian-product of the provided seqs. The provided seqs can be lazy or even
   infinite, and lazy-cross will consume all sequences equally, only consuming more of any sequence
   when all possible combinations at the current level have been exhausted. This can be thought of
   intuitively as a breadth-first search of the cartesian product set."
  [& seqs]
  (letfn [(step [heads tails dim]
            (lazy-seq
             (when (< dim (count tails))
               (let [tail (get tails dim)]
                 (concat (apply cross (assoc heads dim tail))
                         (step (update-in heads [dim] concat tail)
                               tails (inc dim)))))))
          (lazy-cross [seqs level]
            (lazy-seq
             (let [heads (vec (map #(take level %) seqs))
                   tails (vec (map #(take 1 (drop level %)) seqs))]
               (when-not (every? empty? tails)
                 (concat (step heads tails 0)
                         (lazy-cross seqs (inc level)))))))]
    (lazy-cross seqs 0)))
