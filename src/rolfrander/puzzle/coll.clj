(ns rolfrander.puzzle.coll)


(defn map-pairs
  "Calls f for each unique pair in coll, lazy."
  [f coll]
  (for [a-list (take-while (complement empty?) (iterate rest coll))
        :let [a (first a-list)]
        b (rest a-list)]
    (f a b)))

; https://en.wikipedia.org/wiki/Heap%27s_algorithm

(defn permute [v]
  (letfn [(swap [V a b]
            (assoc V
                   a (V b)
                   b (V a)))]

    (loop [stack (vec (repeat (count v) 0))
           i 1
           A v
           output [v]]
      (if (= i (count stack))
        output
        (if (< (stack i) i)
          (let [A (if (even? i)
                    (swap A 0 i)
                    (swap A (stack i) i))
                output (conj output A)]
            (recur (update stack i inc) 1 A output))
          (recur (assoc stack i 0) (inc i) A output))))))

(defn split-by
  "Splits sequence.
   Returns a lazy sequence of seq from coll, starting a new sequence everytime pred change from true to false"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [[xs ys] (split-with pred s)]
       (if (seq xs)
         (cons xs (split-by pred ys))
         (let [!pred (complement pred)
               skip (take-while !pred s)
               others (drop-while !pred s)
               [xs ys] (split-with pred others)]
           (cons (concat skip xs)
                 (split-by pred ys))))))))

(defn every<
  "returns true if the two collections are of equal length and all elements in coll1
   are less than or equal to the corresponding elements in coll2"
  [coll1 coll2]
  (cond
    (and (nil? (seq coll1)) (nil? (seq coll2))) true
    (<= 0 (first coll1) (first coll2)) (recur (next coll1) (next coll2))
    :else false))