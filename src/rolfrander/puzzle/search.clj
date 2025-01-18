(ns rolfrander.puzzle.search
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.tools.logging :as log]))

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
            g-score (assoc {} start 0); gScore := map with default value of Infinity
            f-score (priority-map start (heuristic-fn start)) ; fScore := map with default value of Infinity
            current-longest 0]
       (let [[current _estimate] (peek f-score) ; current := the node in openSet having the lowest fScore value
             estimate 0]
         (when (> estimate current-longest) (log/debug "current longest path:" estimate))
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
