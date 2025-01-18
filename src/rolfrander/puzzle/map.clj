(ns rolfrander.puzzle.map
  (:require [rolfrander.puzzle.num :refer [sign]]
            [rolfrander.puzzle.coll :refer [every<]])
  (:import
   [java.io File]
   [java.awt Color]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]))

(def ^:private neighbours2-4 [[0  1]
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
  [[-1 0 0] [1 0 0]
   [0 -1 0] [0 1 0]
   [0 0 -1] [0 0 1]])

(def ^:private neighbours4-80
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)
        w (range -1 2)
        :when (not-every? (partial = 0) [x y z w])]
    [x y z w]))

(def ^:private neighbours4-8
  [[-1 0 0 0] [1 0 0 0]
   [0 -1 0 0] [0 1 0 0]
   [0 0 -1 0] [0 0 1 0]
   [0 0 0 -1] [0 0 0 1]])

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
  [[-1  1] [0  1]
   [-1  0]       [1  0]
   [-1 -1] [0 -1]])

;; (map #(move [0 0] %) (re-seq #"e|w|ne|se|nw|sw" "nwneweswse"))
; directions for alternating-EW (odd? y)
(def ^:private neighbours-6-odd
  [[0  1] [1  1]
   [-1  0]      [1  0]
   [0 -1] [1 -1]])

;; directions for straight-EW
(def ^:private neighbours-6
  [[-1 1] [0 1]
   [-1 0]       [1 0]
   [0 -1] [1 -1]])

(def ^:private directions-6-ew
  ["nw" "ne"
   "w"       "e"
   "sw" "se"])

(def ^:private directions-6-ns
  ["nw" "n"
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

(def ^:private directions-6-ns-alt ["se" "ne"
                                    "s"       "n"
                                    "sw" "nw"])


(def ^:private directions-4 ["n"
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
                      (general (get (neighbours-n dimensions) grid-type)))]
    (case border
      :infinite infinite-fn
      :ignore   (fn [pos] (filter #(every< % max-dim) (infinite-fn pos)))
      :wrap     (fn [pos] (map #(map mod % max-dim) (infinite-fn pos))))))


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

(defn print-map-png [data w h filename]
  (let [bi (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)]
    (.setColor g Color/BLACK)
    (.fillRect g 0 0 w h)
    (doseq [r data]
      (.setColor g Color/WHITE)
      (.fillRect g (first r) (second r) 1 1))
    (ImageIO/write bi "png" (File. (format "%s.png" filename)))))
