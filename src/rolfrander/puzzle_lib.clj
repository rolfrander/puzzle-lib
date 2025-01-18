(ns rolfrander.puzzle-lib
  (:require [clojure.data.priority-map :refer [priority-map priority-map-by]]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [clojure.string :as str])
  (:import
   [java.io File]
   [java.awt Color]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]))

(def ^:dynamic *debug* "Controls the printing of debuggin-information from this library." false)

(defn get-config 
  "Reads /config. 
   
   Example config-file:
   
   ```
   {
     :session \"session-cookie\"
   }```"
  []
  (with-open [c (java.io.PushbackReader.
                 (io/reader "config"))]
    (read c)))

(defn get-data 
  "Download and cache puzzle input from advent of code. 
   Needs a session cookie stored in `/config` under key `:session`"
  [year day]
  (let [url (format "https://adventofcode.com/%d/day/%d/input" year day)
        local-file (format "resources/advent%d/day%02d.txt" year day)]
    (if (.exists (io/file local-file))
      (do (when *debug* (println "loading input from" local-file))
          (slurp local-file))
      (let [response (http/get url {:cookies {"session" {:value (:session (get-config))}}})]
        (when *debug* (println "loading input from http"))
        (if (not= (:status response) 200)
          (throw (RuntimeException. (str "error getting data: " (:reason-phrase response))))
          (let [body (:body response)]
            (with-open [w (io/writer local-file)]
              (.write w body))
            body))))))

; post data
;Request URL: https://adventofcode.com/2016/day/21/answer
;Request Method: POST
;Status Code: 200 
;cookie: session=53616c7465645f5f...
;origin: https://adventofcode.com
;referer: https://adventofcode.com/2016/day/21
;
;level=1&answer=gbhcefad


(defn str->long 
  "Safe parsing of string to long.
   If s looks like a base-10 number, return a long, otherwise return original string"
  [s]
  (if (and (not (nil? s)) (re-matches #"[+-]?[0-9]+" s))
    (Long/parseLong s)
    s))

(def safe-parse-number str->long) ; backwards compatible alias

(defn char->digit
  "Interprets a char [0-9] as digit"
  [c]
  (- (int c) (int \0)))

(defn digits
  "Returns a seq of each digit in the input as a list of long"
  [number & {:keys [base width]
             :or {base 10 width 1}}]
  (loop [t number
         s '()
         w width]
    (if (and (= 0 t) (<= w 0))
      s
      (recur (quot t base)
             (cons (mod t base) s)
             (dec w)))))

(def siffer digits)

(defn digits->long
  "converts a list of long in a given base to a number"
  [digit & {:keys [base]
            :or {base 10}}]
  (reduce #(+ (* %1 base) %2) digit))

(def siffer->long digits->long)

(defn safe-inc [x]
  (if (nil? x) 1 (inc x)))

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

(defn dijkstra
  "Find shortest path.
   For all nodes, find the shortest path from source to destination. 
   Nodes is a collection of nodes. Source is the start-node.
   Neighbour-fn takes one parameter, v, and returns all neighbours of v. If all nodes
   are not known beforehand, use a-star instead.

   Optional parameters:
   * weight-fn takes two paramenters, u and v, and returns the distance from u to v. 
   Both u and v are taken from nodes. Default is for all weights to be 1.
   * result-type is :dist :prev or :both, specifying returning distance, path or both
   * dest? is a test to whether the destination is reached. If not specified, 
   all distances and all paths are returned"
  [nodes source neighbour-fn & {:keys [weight-fn result-type dest?]
                                :or {weight-fn (constantly 1)
                                     result-type :both
                                     dest? (constantly false)}}]
  ; implemented from wikipedia https://en.wikipedia.org/wiki/Dijkstra's_algorithm
  (letfn [(get-path
           ([prev dest]
            (get-path prev dest []))
           
           ([prev dest p]
            (if (contains? prev dest)
              (recur prev (prev dest) (conj p dest))
              (conj p dest))))]
    (loop [dist (-> (zipmap nodes (repeat 99999999))
                    (assoc source 0))
           prev {}
           Q (into (priority-map) dist)]
      (if (empty? Q)
        (case result-type
          :dist dist
          :prev prev
          :both [dist prev])
        (let [u (first (peek Q))
              Q (pop Q)]
          (if (dest? u)
            (case result-type
              :dist (dist u)
              :prev (get-path prev u)
              :both [(dist u) (get-path prev u)])
            (let [[new-dist new-prev new-q]
                  (->> (filter Q (neighbour-fn u)) ; for each neighbour v of u, still in Q
                       (reduce (fn [[new-dist new-prev q] v]
                                 (let [alt (+ (new-dist u) (weight-fn u v))] ; alt <- dist[u] + length(u,v)
                                   (if (< alt (new-dist v))                ; if alt < dist[v]
                                     [(assoc new-dist v alt)               ;   dist[v] <- alt
                                      (assoc new-prev v u)                 ;   prev[v] <- u
                                      (assoc q v alt)]                     ;   Q.decrease_priority(v, alt)
                                     [new-dist new-prev q])))
                               [dist prev Q]))]
              (recur new-dist new-prev new-q))))))))


(defn a-star 
  "Find the shortest path from start to goal, given a heuristic.
   If the heruistic is admissible (never overestimates 
   the actual distance), a-star is guaranteed to find the shortest path. If the heuristic is consistent, a-star is
   optimally efficient. Consistent means that h(x) ≤ d(x, y) + h(y).
   
   * Start is the start-node, 
   * `goal?` is a function taking one parameter returning true if the goal is reached
   * `heurictic-fn` is a function returning the assumed distance from a node to the destination
   * `paths-fn` take a node as input and returns the possible paths out of that node
   * `neightbour-fn` takes a node and a path as input and returns the target-node of that path
   * `dist-fn` takes a path as input and returns the distand for that path
   * `result-type` is `:path` for returning the nodes traveled through, `:count` for returning the number 
     of nodes, `:last` for returning the last node or `:dist` for returning the shortest known distance from start to goal.
   
   the `paths-fn` can be omitted, then the `neighbour-fn` should take as input a node and return all neighbour-nodes, and
   the `dist-fn` should take two neighbouring nodes as input and return the distance between them. This is the canonical 
   version of the algorithm documented on wikipedia.

   if `puzzle-lib/*debug*` is set to true, the algorithm will report the current longest distance as it goes.
   "
  ([start goal? heuristic-fn neighbour-fn dist-fn result-type]
   (a-star start
           goal?
           heuristic-fn
           (fn [src] (map #(vector src %) (neighbour-fn src)))
           (fn [_src1 [_src2 dest]] dest) ; [_src dest] is returned from the paths-fn over
           (partial apply dist-fn)
           result-type))


  ([start goal? heuristic-fn paths-fn neighbour-fn dist-fn result-type]
   (let [summarize (fn [came-from current start-i aggr-fn]
                     (loop [i start-i
                            current current]
                       (if (not (contains? came-from current))
                         i
                         (recur (aggr-fn i current) (came-from current)))))]
     (loop [;open-set (conj #{} d) ; openSet := {start}
            came-from {}          ; cameFrom := an empty map
            g-score (assoc (if *debug* (priority-map-by >) {}) start 0); gScore := map with default value of Infinity
            f-score (priority-map start (heuristic-fn start)) ; fScore := map with default value of Infinity
            current-longest 0]
       (let [[current _estimate] (peek f-score) ; current := the node in openSet having the lowest fScore value
             estimate (if *debug* (second (peek g-score)) 0)]
         (when (and *debug* (> estimate current-longest)) (println "current longest path:" estimate))
         (cond (nil? current)      ; while openSet is not empty
               nil                    ; return failure

               (goal? current)                              ; if current = goal
               (case result-type
                 :path  (summarize came-from current '() conj) ; return reconstruct_path(cameFrom, current)
                 :count (summarize came-from current   0 (fn [a _b] (inc a)))
                 :dist (g-score current)
                 :last current)

               :else
               (let [f-score (pop f-score)             ; openSet.remove(current)
                     paths (paths-fn current) ; for each neighbour of current
                     [came-from g-score f-score]
                     (reduce (fn [[came-from g-score f-score] path]
                               (let [neighbour (neighbour-fn current path)
                                     tentative-g-score (+ (or (g-score current) 9999) (dist-fn path))] ; tentative_gScore := gScore[current] + d(current, neighbour)
                                 (if (< tentative-g-score (or (g-score neighbour) 9999))          ; if tentative_gScore < gScore[neighbour]
                                   [(assoc came-from neighbour current)                           ; cameFrom[neighbour] := current
                                    (assoc g-score neighbour tentative-g-score)                   ; gScore[neighbour] := tentative_gScore
                                    (assoc f-score neighbour (+ tentative-g-score (heuristic-fn neighbour)))] ; fScore[neighbour] := tentative_gScore + h(neighbour)
                                   [came-from g-score f-score])))
                             [came-from g-score f-score]
                             paths)]
                 (recur came-from g-score f-score (max current-longest estimate)))))))))

(defn interpreter
  "General interpreter logic.
   
   Input is assumed to be a string with lines formatted as `mnemonic parameters...`, with parameters separated by space.
   The CPU-function should have the following signature: `instruction-pointer state mnemonic parameters` and return a new state. The state
   should not include instruction-pointer, that is covered by the interpreter. If the instruction is a jump, instead return
   `{:jmp relative-position}`."
  [input cpu-function start-state]
  (let [parse-line (fn [line] (map safe-parse-number (str/split line #" +")))
        program (cond (vector? input) input
                      (coll? input) (vec input)
                      :else (mapv parse-line (str/split-lines input)))
        last-ip (dec (count program))]
    (loop [state start-state
           ip 0
           cnt 0]
      (if (> ip last-ip)
        {:state state :ip ip :count cnt}
        (let [[mnemonic & params] (get program ip)] 
          (when *debug* (printf "instruction: %s %s, state: %s\n" mnemonic params state))
          (let [next-state (cpu-function ip state mnemonic params)]
            (cond (= next-state :hlt)
                  {:state state :ip ip :count cnt}

                  (contains? next-state :jmp)
                  (recur state (+ ip (:jmp next-state)) (inc cnt))

                  :else
                  (recur next-state (inc ip) (inc cnt)))))))))

(def ^:private neighbours2-4 [     [0  1]
                    [-1 0]    [1 0]
                         [0 -1]])


(def ^:private neighbours2-8 [[-1  1] [0  1] [1  1]
                    [-1  0]        [1  0]
                    [-1 -1] [0 -1] [1 -1]])

(def ^:private neighbours3-26
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)
        :when (not-every? (partial = 0) [x y z])]
    [x y z 0]))

(def ^:private neighbours3-6
  [[-1 0 0][1 0 0]
   [0 -1 0][0 1 0]
   [0 0 -1][0 0 1]])

(def ^:private neighbours4-80
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)
        w (range -1 2)
        :when (not-every? (partial = 0) [x y z w])]
    [x y z w]))

(def ^:private neighbours4-8
  [[-1 0 0 0][1 0 0 0]
   [0 -1 0 0][0 1 0 0]
   [0 0 -1 0][0 0 1 0]
   [0 0 0 -1][0 0 0 1]])

(def ^:private neighbours2
  {:sq-4 neighbours2-4
   :sq-8 neighbours2-8})

(def ^:private neighbours3
  {:sq-4 neighbours3-6
   :sq-8 neighbours3-26})

(def ^:private neighbours4
  {:sq-4 neighbours4-8
   :sq-8 neighbours4-80})

(def ^:private neighbours-n
  [nil nil neighbours2 neighbours3 neighbours4])


; directions for alternating-EW for (even? y)
(def ^:private neighbours-6-even
  [   [-1  1] [0  1]
   [-1  0]       [1  0]
      [-1 -1] [0 -1]])

;; (map #(move [0 0] %) (re-seq #"e|w|ne|se|nw|sw" "nwneweswse"))
; directions for alternating-EW (odd? y)
(def ^:private neighbours-6-odd
  [   [0  1] [1  1]
   [-1  0]      [1  0]
      [0 -1] [1 -1]])

;; directions for straight-EW
(def ^:private neighbours-6
  [   [-1 1] [0 1]
   [-1 0]       [1 0]
      [0 -1] [1 -1]])

(def ^:private directions-6-ew
  [  "nw" "ne"
   "w"       "e"
     "sw" "se"])

(def ^:private directions-6-ns
  [  "nw" "n"
   "sw"    "ne"
     "s" "se"])

; a bit strange because dimensions are mirrored from the -EW matrices
; x is even:
; nw = -1  0
; n  =  0  1
; ne =  1  0
; sw = -1 -1
; s  =  0 -1
; se =  1 -1

; x is odd:
; nw = -1  1
; n  =  0  1
; ne =  1  1
; sw = -1  0
; s  =  0 -1
; se =  1  0

(def ^:private directions-6-ns-alt [  "se" "ne"
                          "s"       "n"
                            "sw" "nw"])


(def ^:private directions-4 [  "n" 
                   "w" "e"
                     "s"])
(def ^:private directions-8 ["nw" "n" "ne"
                    "w"      "e"
                   "sw" "s" "se"])


(defn switch-xy [n]
  (doall (map (fn [[x y]] [y x]) n)))

(def ^:private directions-maps
  {:sq-4 (zipmap directions-4 neighbours2-4)
   :sq-8 (zipmap directions-8 neighbours2-8)
   :hex-ew-str (zipmap directions-6-ew neighbours-6)
   :hex-ns-str (zipmap directions-6-ns neighbours-6)
   :hex-ew-alt [(zipmap directions-6-ew neighbours-6-even)
                (zipmap directions-6-ew neighbours-6-odd)]
   :hex-ns-alt [(zipmap directions-6-ns-alt (switch-xy neighbours-6-even))
                (zipmap directions-6-ns-alt (switch-xy neighbours-6-odd))]})

(defn every< 
  "returns true if the two collections are of equal length and all elements in coll1
   are less than or equal to the corresponding elements in coll2"
  [coll1 coll2]
  (cond
    (and (nil? (seq coll1)) (nil? (seq coll2))) true
    (<= 0 (first coll1) (first coll2)) (recur (next coll1) (next coll2))
    :else false))

; (defn abs [x] (Math/abs x))

(defn sign [x] (cond (= x 0) 0
                     (> x 0) 1
                     :else -1))

(defn hexagon-manhattan-distance
  "Manhattan distance in a hex grid.
   
   Only works in straight grids, not alternating.
   https://stackoverflow.com/questions/5084801/manhattan-distance-between-tiles-in-a-hexagonal-grid"
  [[x y]]
  (if (= (sign x) (sign y))
    (abs (+ x y))
    (max (abs x) (abs y))))

(defn move-fn
  "Creates a function for moving in compass-direction.
   All of the explanations assume that coordinates are `[x y]` (that is, the first coordinate moves in the east-west-direction).
   The returned function takes a position and a direction as input. The direction is one of n, s, e, w, ne, se, nw, sw.
      
   See neighbours-fn for definition of parameters."

  [grid-type border & {:keys [max-dim]}]
  (assert (#{:hex-ew-alt :hex-ns-alt :hex-ew-str :hex-ns-str :sq-4 :sq-8} grid-type)
          (str "unknown grid-type " grid-type))
  (assert (#{:infinite :ignore :wrap} border)
          (str "unknown border " border))
  (assert (or (= border :infinite) (seq max-dim))
          "if border is :ignore or :wrap, max-dim must be specified")
  (let [infinite-fn (case grid-type
                      :hex-ns-alt
                      (let [[dir-even dir-odd] (directions-maps :hex-ns-alt)]
                        (fn [[x _y :as pos] direction]
                          (map + pos (if (odd? x)
                                       (dir-odd direction)
                                       (dir-even direction)))))

                      :hex-ew-alt
                      (let [[dir-even dir-odd] (directions-maps :hex-ew-alt)]
                        (fn [[_x y :as pos] direction]
                          (map + pos (if (odd? y)
                                       (dir-odd direction)
                                       (dir-even direction)))))

                      (let [dir (directions-maps grid-type)]
                        (fn [pos direction] (map + pos (dir direction)))))]
    (case border
      :infinite (comp doall infinite-fn)
      :ignore   (fn [pos direction] (let [newpos (doall (infinite-fn pos direction))] (when (every< newpos max-dim) newpos)))
      :wrap     (fn [pos direction] (doall (map mod (infinite-fn pos direction) max-dim))))))

(defn neighbours-fn
  "Creates a function for calculating neighbours in a grid.
   All of the explanations assume that coordinates are `[x y]` (that is, the first coordinate moves in the east-west-direction), but
   this doesn't really matter for the calculations. The hex-ns and hex-ew are equal but with axes reversed.
   
   Grid-type is one of:
   * `:sq-4` neighbours are north, south, east, west
   * `:sq-8` as fort `sq-4`, but also diagonals nw, sw, ne, se
   * `:hex-ns-str` grid of 6-sided tiles where it is possible to move straight north or south
   * `:hex-ew-str` as above, but where the straight movements are east and west
   * `:hex-ns-alt` y-lines are squiggly instead of straight NE
   * `:hex-ew-alt` x-lines are squiggly inste3ad fo straight NE

   Border defines what happens when reaching the end of the map:
   * `:infinite` means there is no end
   * `:ignore` means positions larger than max-dim or smaller than 0 are ignored
   * `:wrap` means wrapping around from max-dim to 0
   
   Max-dim is an array of maximum values in each dimension (note that maximum is interpreted differently in :ignore and :wrap).
   Must be specified if border is not `:infinite`.
   
   Dimensions is the number of dimensions. Supported values are 2, 3, 4. Only works for square grids."
  [grid-type border & {:keys [dimensions max-dim]
                       :or {dimensions 2}}]
  (assert (#{:hex-ew-alt :hex-ns-alt :hex-ew-str :hex-ns-str :sq-4 :sq-8} grid-type)
          (str "unknown grid-type " grid-type))
  (assert (#{:infinite :ignore :wrap} border)
          (str "unknown border " border))
  (assert (or (nil? max-dim) (= (count max-dim) dimensions))
          "max-dim must have dim number of elements")
  (assert (or (= border :infinite) (seq max-dim))
          "if border is :ignore or :wrap, max-dim must be specified")
  (let [general (fn [n] (fn [pos] (map #(map + pos %) n)))
        infinite-fn (case grid-type
                      :hex-ew-str (general neighbours-6)
                      :hex-ns-str (general neighbours-6)
                      :hex-ew-alt (fn [[_x y :as pos]]
                                    (if (odd? y)
                                      (map #(map + pos %) neighbours-6-odd)
                                      (map #(map + pos %) neighbours-6-even)))
                      :hex-ns-alt (fn [[x _y :as pos]]
                                    (if (odd? x)
                                      (map #(map + pos %) (switch-xy neighbours-6-odd))
                                      (map #(map + pos %) (switch-xy neighbours-6-even))))
                      ; :sq-4 and :sq-8 use the same code, but different neighbour-matrices
                      (general (get (neighbours-n dimensions) grid-type))
                      )]
    (case border
      :infinite infinite-fn
      :ignore   (fn [pos] (filter #(every< % max-dim) (infinite-fn pos)))
      :wrap     (fn [pos] (map #(map mod % max-dim) (infinite-fn pos))))))

(defn ^booleans prime-sieve [cnt]
  (let [result (boolean-array cnt true)]
    (aset-boolean result 0 false)
    (aset-boolean result 1 false)
    (doseq [i (range 2 (Math/sqrt cnt))]
      (when (aget result i)
        (doseq [j (range (* i i) cnt i)]
          (aset-boolean result j false))))
    result))

(defn get-prime-sieve
  "returns is-prime? for numbers up to max"
  [max]
  (let [primes ^booleans (prime-sieve max)]
    (fn [look-for-number]
      (aget primes look-for-number))))

(defn gcd [^long a ^long b]
  (if (= b 0) a
      (recur b (long (mod a b)))))

(defn mod-inverse [a n]
  (loop [t 0
         new-t 1
         r n
         new-r a]
    (if (zero? new-r)
      (if (> r 1)
        nil
        (if (< t 0) (+ t n) t))
      (let [q (quot r new-r)]
        (recur new-t (mod (- t (* q new-t)) n)
               new-r (mod (- r (* q new-r)) n))))))

(defn count-bits 
  "counts number of bits in input.
   
   Works for ints up to 8 bits."
  [^long i]
  (let [i (- i (bit-and (bit-shift-right i 1)  0x55555555))
        i (+   (bit-and                  i     0x33333333)
               (bit-and (bit-shift-right i 2)  0x33333333))
        i (bit-and (+ i (bit-shift-right i 4)) 0x0F0F0F0F)
        i (bit-shift-right                (* i 0x01010101) 24)]
    i))

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


(defn parse-map
  "input is a string with lines of the same length consisting of '.' meaning a blank space and other markings of interest. Returns a map 
  {
    :width
    :height
    :type type
    :markings { } ; mapping from mark-char to list of [x y] positions
   
   Available types:
    :grid - returns a vector of vectors
    :by-type - a map of cell-types containing a list of coordinates
    :by-coord - a map of coordinates to cell-type
   tx is an optional transform-function for marks
  }"
  [in & {:keys [type tx] :or {type :by-type tx identity}}]
  (let [by-type (fn [markings row y]
                  (->> (map-indexed #(vector %2 [%1 y]) row)
                       (reduce #(update %1 (tx (first %2)) conj (second %2))
                               markings)))
        grid (fn [markings row _y]
               (conj markings (mapv tx row)))
        by-coord (fn [markings row y]
                   (->> (map-indexed #(vector %2 [%1 y]) row)
                        (reduce #(assoc %1 (second %2) (tx (first %2)))
                                markings)))
        parse-row (case type
                    :grid grid
                    :by-type by-type
                    :by-coord by-coord
                    by-type ; default
                    )]
    (loop [[row & rows] (str/split-lines in)
           y 0
           markings (if (= type :grid) [] {})
           width 0]
      (if (nil? row)
        {:width width
         :height y
         :type type
         :markings markings}
        (recur rows (inc y)
               (parse-row markings row y)
               (count row))))))

(defn convert-map
  "converts between types as returned by parse-map. See parse-map for details.
   Does not need :width or :height to be set. If no type is given, tries to guess.
   Also accepts markings not wrapped in a map-type map. The given type is the output-type.
   Normalizes all coordinates to be non-negative"
  [in & {:keys [type tx] :or {type :by-type tx identity}}]
  (let [; wrap in map with :markings
        in (if (and (map? in) (contains? in :markings))
             in
             {:markings in})
        ; ensure type is set
        in (cond (contains? in :type) in

                 (and (vector? (:markings in))
                      (vector? (first (:markings in))))
                 (assoc in :type :grid)

                 (and (map? (:markings in))
                      (seq (first (vals (:markings in)))))
                 (assoc in :type :by-type)

                 (and (map? (:markings in))
                      (seq (first (keys (:markings in))))
                      (= 2 (count (first (keys (:markings in))))))
                 (assoc in :type :by-coord)

                 :else
                 (throw (IllegalArgumentException. "unable to recognise map sent as input to convert-map")))
        ; find min and max values
        [max-x max-y min-x min-y] (case (:type in)
                                    :by-coord (for [k [max-key min-key] d [first second]]
                                                (d (apply k d (keys (:markings in)))))
                                    :by-type  (for [k [max-key min-key] d [first second]]
                                                (d (apply k d (apply concat (vals (:markings in))))))
                                    :grid [(dec (count (apply max-key count (:markings in))))
                                           (dec (count in))
                                           0
                                           0])
        in (assoc in
                  :width    (inc (- max-x min-x))
                  :height   (inc (- max-y min-y)))

        transpose-coords (fn [coords] (map - coords [min-x min-y]))]
    
    (assoc in
           :type type
           :markings
           (case [(:type in) type]
             [:grid :grid]         (mapv #(mapv tx %) (:markings in))
             [:grid :by-coord]     (into {} (for [y (range (count (:markings in)))
                                                  x (range (count (get (:markings in) y)))]
                                              [[x y] (tx (get-in (:markings in) [y x]))]))
             [:grid :by-type]      (reduce (fn [ret [x y]]
                                             (update ret
                                                     (tx (get-in (:markings in)
                                                                 [y x]))
                                                     conj [x y]))
                                           {}
                                           (for [y (range (count (:markings in)))
                                                 x (range (count (get (:markings in) y)))]
                                             [x y]))

             [:by-coord :by-coord] (reduce-kv #(assoc %1 (transpose-coords %2) (tx %3))
                                              {} (:markings in))
             [:by-coord :by-type]  (reduce-kv #(update %1 (tx %3) conj (transpose-coords %2))
                                              {} (:markings in))
             [:by-coord :grid]     (mapv (fn [y] (mapv (fn [x] (tx ((:markings in) [x y])))
                                                       (range min-x (inc max-x))))
                                         (range min-y (inc max-y)))

             [:by-type :by-type]   (reduce-kv (fn [ret k v]
                                                (assoc ret (tx k) (map transpose-coords v)))
                                              {} (:markings in))
             [:by-type :by-coord]  (reduce-kv (fn [ret k v]
                                                (reduce #(assoc %1 %2 (tx k))
                                                        ret v))
                                              {} (:markings in))
             [:by-type :grid]      (let [ret (vec (repeat (:height in) (vec (repeat (:width in) nil))))]
                                     (reduce-kv (fn [ret mark coord-list]
                                                  (reduce (fn [ret [x y]]
                                                            (assoc-in ret [(- y min-y) (- x min-x)] (tx mark)))
                                                          ret coord-list))
                                                ret (:markings in)))

             (throw (UnsupportedOperationException. (str "conversion from " (:type in) " to " type " not implemented")))))))

(defn print-map [m & {:keys [from-bottom unknown-mark] :or {from-bottom false unknown-mark \.}}]
  (let [{:keys [type height width markings]} m
        marks (if (= type :by-type)
                (reduce (fn [res marks]
                          (let [symbol (first marks)]
                            (reduce #(assoc %1 %2 symbol)
                                    res
                                    (second marks))))
                        {} markings)
                markings)]
    (if (= type :grid)
      (doseq [row (if from-bottom
                    (rseq markings)
                    markings)]
        (println (apply str row)))

      (doseq [y (if from-bottom
                  (range height)
                  (range (dec height) -1 -1))
              x (range width)]
        (when (and (= x 0) (> y 0)) (println))
        (print (get marks [x y] unknown-mark))))))

(defn draw-map [m]
  (let [max-x (:width m)
        max-y (:height m)
        marks (reduce (fn [res marks]
                        (let [symbol (first marks)]
                          (reduce #(assoc %1 %2 symbol)
                                  res
                                  (second marks))))
                      {}
                      (:markings m))]
    (doseq [y (range max-y)
            x (range max-x)]
      (when (and (= x 0) (> y 0)) (println))
      (print (get marks [x y] \.)))))

(defn print-png [data w h filename]
  (let [bi (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)]
    (.setColor g Color/BLACK)
    (.fillRect g 0 0 w h)
    (doseq [r data]
      (.setColor g Color/WHITE)
      (.fillRect g (first r) (second r) 1 1))
    (ImageIO/write bi "png" (File. (format "%s.png" filename)))))
