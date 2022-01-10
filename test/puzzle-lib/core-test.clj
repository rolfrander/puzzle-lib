(ns puzzle-lib.test
  (:require [clojure.test :as test]
            [puzzle-lib :refer :all]))

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
                  {"sandvika" "asker" "oslo" "sandvika" "lillestrøm" "oslo" "moss" "oslo" "kjeller" "lillestrøm" "fetsund" "lillestrøm" "sarpsborg" "moss" "fredrikstad" "moss"}]
      ]
  

  (test/deftest test-dijkstra
    (test/is (= paths-count
                (dijkstra (keys neighbours) "asker" neighbours)))
    (test/is (= paths-dist
                (dijkstra (keys neighbours) "asker" neighbours :weight-fn #(distance (conj #{} %1 %2)))))
    (test/are [final-dest]
              (= (get-in paths-dist [0 final-dest])
                 (a-star "asker"
                         (partial = final-dest)
                         (constantly 1)
                         neighbours
                         (comp distance (partial conj #{}))
                         :dist))
      "fredrikstad"
      "lillestrøm"
      "kjeller")
    (test/are [final-dest]
              (= (get-in paths-count [0 final-dest])
                 (a-star "asker"
                         (partial = final-dest)
                         (constantly 1)
                         neighbours
                         (constantly 1) ; paths-count is computed by counting, not measured distance, so for the test to pass, the distance must be constant
                         :count))
      "fredrikstad"
      "lillestrøm"
      "kjeller"))
     )

(binding [puzzle-lib/*debug* false]
  (test/run-all-tests #"puzzle-lib.test"))