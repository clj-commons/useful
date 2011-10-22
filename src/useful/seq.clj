(ns useful.seq
  (:use [useful.fn :only [decorate]]))

(defn find-first
  "Returns the first item of coll where (pred item) returns logical true."
  [pred coll]
  (first (filter pred coll)))

(defn find-with
  "Returns the val corresponding to the first key where (pred key) returns true."
  [pred keys vals]
  (->> (map vector keys vals)
       (find-first (comp pred first))
       last))

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

(defn include?
  "Check if val exists in coll."
  [val coll]
  (some (partial = val) coll))

(defn zip
  "Returns a lazy sequence of vectors of corresponding items from each collection. If one collection
   is longer than the others, the missing items will be filled in with nils."
  [& colls]
  (lazy-seq
   (when (some seq colls)
     (cons (vec (map first colls))
           (apply zip (map rest colls))))))

(defn insert
  "Inserts a seq of items into coll at position n."
  [items n coll]
  (let [[before after] (split-at n coll)]
    (concat before items after)))

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

(defn alternates
  "Split coll into 'threads' subsequences (defaults to 2), feeding
  each alternately from the input sequence. Effectively the inverse of
  interleave:

  (alternates 3 (range 9))
  ;=> ((0 3 6) (1 4 7) (2 5 8))"
  ([coll] (alternates 2 coll))
  ([threads coll]
     (lazy-seq
      (when (seq coll)
        (apply map list (partition threads coll))))))

(defmacro lazy-loop
  "Provide a simplified version of lazy-seq to eliminate
  boilerplate. Arguments are as to the built-in (loop...recur),
  and (lazy-recur) will be defined for you. However, instead of doing
  actual tail recursion, lazy-recur trampolines through lazy-seq. In
  addition to enabling laziness, this means you can call lazy-recur
  when not in the tail position."
  [bindings & body]
  (let [inner-fn 'lazy-recur
        [names values] (alternates bindings)]
    `((fn ~inner-fn
        ~(vec names)
        (lazy-seq
         ~@body))
      ~@values)))

(defn unfold
  "Traditionally unfold is the 'opposite of reduce': it turns a single
  seed value into a (possibly infinite) lazy sequence of output
  values.

  Next and done? are functions that operate on a seed. next should
  return a pair, [value new-seed]; the value half of the pair is
  inserted into the resulting list, while the new-seed is used to
  continue unfolding. Notably, the value is never passed as an
  argument to either next or done?.

  If done? is omitted, the sequence will be unfolded forever, for
  example
  (defn fibs []
    (unfold (fn [[a b]]
              [a [b (+ a b)]])
            [0 1]))"
  ([next seed]
     (unfold next (constantly false) seed))
  ([next done? seed]
     (lazy-loop [seed seed]
       (when-not (done? seed)
         (let [[value new-seed] (next seed)]
           (cons value
                 (lazy-recur new-seed)))))))

(defn take-shuffled
  "Lazily take (at most) n elements at random from coll, without
  replacement. For n=1, this is equivalent to rand-nth; for n>=(count
  coll) it is equivalent to shuffle.

  Clarification of \"without replacement\": each index in the original
  collection is chosen at most once. Thus if the original collection
  contains no duplicates, neither will the result of this
  function. But if the original collection contains duplicates, this
  function may include them in its output: it does not do any
  uniqueness checking aside from being careful not to use the same
  index twice."
  [n coll]
  (let [coll (vec coll)
        n (min n (count coll))]
    (take n
          (lazy-loop [coll coll]
            (let [idx (rand-int (count coll))
                  val (coll idx)
                  coll (-> coll
                           (assoc idx (peek coll))
                           pop)]
              (cons val (lazy-recur coll)))))))

(defn foldr
  [f start coll]
  (reduce #(f %2 %1) start (reverse coll)))

(defmacro lazy
  "Return a lazy sequence of the passed-in expressions. Each will be evaluated
  only if necessary."
  [& exprs]
  `(map force (list ~@(for [expr exprs]
                        `(delay ~expr)))))

(defn partition-between
  "Partition an input seq into multiple sequences, as with partition-by.
   Walks the collection two at a time, calling (split? [a b]) for each pair.
   Any time split? returns truthy, the partition containing a ends, and a new
   one containing b begins. Note that the split? predicate should not take two
   arguments, but instead a single argument, a pair.

   Like partition-by, a lazy sequence of paritions is returned, but the
   partitions themselves are eager.

   For example, to cause each nil to be folded into the next partition:
   (partition-between (fn [[a b]] (not (nil? a))) '[1 nil nil 2 nil 3])
   => ([1] [nil nil 2] [nil 3])"
  [split? coll]
  (lazy-seq
   (when-let [[x & more] (seq coll)]
     (lazy-loop [items [x], coll more]
       (if-let [[x & more] (seq coll)]
         (if (split? [(peek items) x])
           (cons items (lazy-recur [x] more))
           (lazy-recur (conj items x) more))
         [items])))))
