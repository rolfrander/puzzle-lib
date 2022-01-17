(ns rolfrander.puzzle-lib-test
  (:require [clojure.test :refer [deftest is are run-all-tests]]
            [rolfrander.puzzle-lib :refer :all]))

(let [neighbours {"oslo"       ["sandvika" "lillestrøm" "moss" "kjeller"]
                  "sandvika"   ["oslo" "asker"]
                  "asker"      ["sandvika"]
                  "lillestrøm" ["fetsund" "kjeller"]
                  "fetsund"    ["lillestrøm"]
                  "kjeller"    ["lillestrøm" "oslo"]
                  "moss"       ["sarpsborg" "fredrikstad" "oslo"]
                  "sarpsborg"  ["fredrikstad" "moss"]
                  "fredrikstad" ["sarpsborg" "moss"]}
      distance {#{"lillestrøm" "fetsund"} 3
                #{"oslo" "kjeller"} 15
                #{"fredrikstad" "sarpsborg"} 20
                #{"moss" "oslo"} 80
                #{"sarpsborg" "moss"} 30
                #{"fredrikstad" "moss"} 25
                #{"kjeller" "lillestrøm"} 2
                #{"lillestrøm" "oslo"} 12
                #{"sandvika" "oslo"} 22
                #{"asker" "sandvika"} 18}

      paths-count [{"kjeller" 3 "asker" 0 "fredrikstad" 4 "lillestrøm" 3 "sandvika" 1 "sarpsborg" 4 "moss" 3 "oslo" 2 "fetsund" 4}
                   {"sandvika" "asker" "oslo" "sandvika" "lillestrøm" "oslo" "moss" "oslo" "kjeller" "oslo" "fetsund" "lillestrøm" "sarpsborg" "moss" "fredrikstad" "moss"}]
      paths-dist [{"kjeller" 54 "asker" 0 "fredrikstad" 145 "lillestrøm" 52 "sandvika" 18 "sarpsborg" 150 "moss" 120 "oslo" 40 "fetsund" 55}
                  {"sandvika" "asker" "oslo" "sandvika" "lillestrøm" "oslo" "moss" "oslo" "kjeller" "lillestrøm" "fetsund" "lillestrøm" "sarpsborg" "moss" "fredrikstad" "moss"}]]


  (deftest dijkstra-test
    (is (= paths-count
           (dijkstra (keys neighbours) "asker" neighbours)))
    (is (= paths-dist
           (dijkstra (keys neighbours) "asker" neighbours :weight-fn #(distance (conj #{} %1 %2)))))
    (are [final-dest]
         (= (get-in paths-dist [0 final-dest])
            (a-star "asker"
                    (partial = final-dest)
                    (constantly 1)
                    neighbours
                    (comp distance (partial conj #{}))
                    :dist))
      "fredrikstad"
      "lillestrøm"
      "kjeller"))
  (deftest a-star-test
    (are [final-dest]
         (= (get-in paths-count [0 final-dest])
            (a-star "asker"
                    (partial = final-dest)
                    (constantly 1)
                    neighbours
                    (constantly 1) ; paths-count is computed by counting, not measured distance, so for the test to pass, the distance must be constant
                    :count))
      "fredrikstad"
      "lillestrøm"
      "kjeller")))

(deftest neighbours-6-test
  (are [grid origin result] (= result (into #{} ((neighbours-fn grid :infinite) origin)))
    :hex-ew-str [0 0] #{[-1 1] [-1 0] [0 1] [0 -1] [1 0] [1 -1]}
    :hex-ew-str [1 3] #{[0 4] [0 3] [1 4] [1 2] [2 3] [2 2]}
    :hex-ns-str [0 0] #{[-1 1] [-1 0] [0 1] [0 -1] [1 0] [1 -1]}
    :hex-ns-str [1 3] #{[0 4] [0 3] [1 4] [1 2] [2 3] [2 2]}
    :hex-ew-alt [0 0] #{[-1 -1] [-1 0] [0 -1] [-1 1] [1 0] [0 1]}
    :hex-ew-alt [1 0] #{[0 -1] [0 0] [1 -1] [0 1] [2 0] [1 1]}
    :hex-ew-alt [0 1] #{[0 0] [-1 1] [1 0] [0 2] [1 1] [1 2]}
    :hex-ns-alt [0 0] #{[0 -1] [-1 -1] [1 -1] [-1 0] [1 0] [0 1]}
    :hex-ns-alt [0 1] #{[0 0] [-1 0] [1 0] [-1 1] [1 1] [0 2]}))

(deftest neighbours-6-wrap-test
  (are [grid origin result] (= result (into #{} ((neighbours-fn grid :wrap :max-dim [2 3]) origin)))
    :hex-ew-str [1 3] #{[0 1] [0 0] [1 1] [1 2] [0 2]}
    ))

(deftest neighbours-4-test
  (are [origin result] (= result (into #{} ((neighbours-fn :sq-4 :infinite) origin)))
    [0 0] #{[-1 0][1 0][0 -1][0 1]}
    [3 4] #{[ 2 4][4 4][3 3] [3 5]}
    ))

(deftest neighbours-4-clip-test
  (are [origin result] (= result (into #{} ((neighbours-fn :sq-4 :ignore :max-dim [4 4]) origin)))
    [3 4] #{[2 4] [4 4] [3 3] }
    [0 0] #{[1 0] [0 1]}))

(deftest neighbours-3d-test
  (are [origin border result] (= result (into #{} ((neighbours-fn :sq-4 border :dimensions 3 :max-dim [4 5 6]) origin)))
    [3 4 5] :infinite #{[2 4 5] [4 4 5] [3 3 5] [3 5 5] [3 4 4] [3 4 6]}
    [3 4 5] :wrap     #{[2 4 5] [0 4 5] [3 3 5] [3 0 5] [3 4 4] [3 4 0]}
    [4 5 6] :ignore   #{[3 5 6] [4 4 6] [4 5 5]}))

(deftest neighbours-3d-diag-test
  (are [origin result] (= result (into #{} ((neighbours-fn :sq-8 :infinite :dimensions 3) origin)))
    [3 4 5]  #{[2 5 4][3 5 4][4 5 4] [2 5 5][3 5 5][4 5 5] [2 5 6][3 5 6][4 5 6]
               [2 4 4][3 4 4][4 4 4] [2 4 5]       [4 4 5] [2 4 6][3 4 6][4 4 6]
               [2 3 4][3 3 4][4 3 4] [2 3 5][3 3 5][4 3 5] [2 3 6][3 3 6][4 3 6]
               }
))

(deftest move-test
  (are [origin direction grid border max-dim result] (= result ((move-fn grid border :max-dim max-dim) origin direction))
    [0 0] "n" :sq-4 :infinite nil [0 1]
    [0 0] "s" :sq-4 :infinite nil [0 -1]
    [0 0] "e" :sq-4 :infinite nil [1 0]
    [0 0] "w" :sq-4 :infinite nil [-1 0]

    [0 0] "n" :sq-8 :infinite nil [0 1]
    [0 0] "s" :sq-8 :infinite nil [0 -1]
    [0 0] "e" :sq-8 :infinite nil [1 0]
    [0 0] "w" :sq-8 :infinite nil [-1 0]

    [0 0] "nw" :sq-8 :infinite nil [-1  1]
    [0 0] "sw" :sq-8 :infinite nil [-1 -1]
    [0 0] "ne" :sq-8 :infinite nil [1 1]
    [0 0] "se" :sq-8 :infinite nil [1 -1]

    [9 9] "n" :sq-8 :ignore [9 9] nil
    [9 9] "s" :sq-8 :ignore [9 9] [9 8]
    [9 9] "se" :sq-8 :ignore [9 9] nil
    [0 0] "w" :sq-8 :ignore [9 9] nil

    [9 9] "n" :sq-8 :wrap [10 10] [9 0]
    [9 9] "s" :sq-8 :wrap [10 10] [9 8]
    [9 9] "se" :sq-8 :wrap [10 10] [0 8]
    [0 0] "w" :sq-8 :wrap [10 10] [9 0]

    [0 0] "nw" :hex-ew-alt :infinite nil [-1  1]
    [0 0] "ne" :hex-ew-alt :infinite nil [0  1]
    [0 0]  "e" :hex-ew-alt :infinite nil [1  0]
    [0 0] "se" :hex-ew-alt :infinite nil [0 -1]
    [0 0] "sw" :hex-ew-alt :infinite nil [-1 -1]
    [0 0]  "w" :hex-ew-alt :infinite nil [-1  0]

    [0 1] "nw" :hex-ew-alt :infinite nil [0  2]
    [0 1] "ne" :hex-ew-alt :infinite nil [1  2]
    [0 1]  "e" :hex-ew-alt :infinite nil [1  1]
    [0 1] "se" :hex-ew-alt :infinite nil [1  0]
    [0 1] "sw" :hex-ew-alt :infinite nil [0  0]
    [0 1]  "w" :hex-ew-alt :infinite nil [-1  1]

    [1 0] "nw" :hex-ew-alt :infinite nil [0  1]
    [1 0] "ne" :hex-ew-alt :infinite nil [1  1]
    [1 0]  "e" :hex-ew-alt :infinite nil [2  0]
    [1 0] "se" :hex-ew-alt :infinite nil [1 -1]
    [1 0] "sw" :hex-ew-alt :infinite nil [0 -1]
    [1 0]  "w" :hex-ew-alt :infinite nil [0  0]

    [0 0] "nw" :hex-ns-alt :infinite nil [-1  0]
    [0 0] "n"  :hex-ns-alt :infinite nil [0  1]
    [0 0] "ne" :hex-ns-alt :infinite nil [1  0]
    [0 0] "se" :hex-ns-alt :infinite nil [1 -1]
    [0 0] "s"  :hex-ns-alt :infinite nil [0 -1]
    [0 0] "sw" :hex-ns-alt :infinite nil [-1 -1]

    [0 1] "nw" :hex-ns-alt :infinite nil [-1  1]
    [0 1] "n"  :hex-ns-alt :infinite nil [0  2]
    [0 1] "ne" :hex-ns-alt :infinite nil [1  1]
    [0 1] "se" :hex-ns-alt :infinite nil [1  0]
    [0 1] "s"  :hex-ns-alt :infinite nil [0  0]
    [0 1] "sw" :hex-ns-alt :infinite nil [-1  0]

    [1 0] "nw" :hex-ns-alt :infinite nil [0  1]
    [1 0] "n"  :hex-ns-alt :infinite nil [1  1]
    [1 0] "ne" :hex-ns-alt :infinite nil [2  1]
    [1 0] "se" :hex-ns-alt :infinite nil [2  0]
    [1 0] "s"  :hex-ns-alt :infinite nil [1 -1]
    [1 0] "sw" :hex-ns-alt :infinite nil [0  0]

    [0 0] "nw" :hex-ew-str :infinite nil [-1  1]
    [0 0] "ne" :hex-ew-str :infinite nil [0  1]
    [0 0]  "e" :hex-ew-str :infinite nil [1  0]
    [0 0] "se" :hex-ew-str :infinite nil [1 -1]
    [0 0] "sw" :hex-ew-str :infinite nil [0 -1]
    [0 0]  "w" :hex-ew-str :infinite nil [-1  0]

    [0 0] "nw" :hex-ns-str :infinite nil [-1  1]
    [0 0] "n"  :hex-ns-str :infinite nil [0  1]
    [0 0] "ne" :hex-ns-str :infinite nil [1  0]
    [0 0] "se" :hex-ns-str :infinite nil [1 -1]
    [0 0] "s"  :hex-ns-str :infinite nil [0 -1]
    [0 0] "sw" :hex-ns-str :infinite nil [-1 0]
    ))

(deftest char->digit-test
  (are [from to] (= to (char->digit from))
       \0 0
       \1 1
       \2 2
       \3 3
       \4 4
       \5 5
       \6 6
       \7 7
       \8 8
       \9 9))

(deftest prime-test
  (are [prime] (is-prime? prime)
    2 3 5 7 11 13
    607
    733
    859
    997
    1093
    1213
    1303
    1439
    1543
    1627
    1753)
  (are [not-prime] (not (is-prime? not-prime))
    4 6 8 8 10 12 14 15 16
    (* 7 11)
    (* 13 17)
    (* 17 23)))

(deftest gcd-test
  (are [a b res] (and (= res (gcd a b)) (= res (gcd b a)))
    (* 13 17) (* 17 23) 17
    (* 3 5 107) (* 7 11 107) 107
    (* 3 41 59) (* 5 41 43) 41
    (* 2 3 41 59) (* 2 5 41 43) (* 2 41)
    ))

(binding [*debug* false]
  (run-all-tests #"puzzle-lib.test"))