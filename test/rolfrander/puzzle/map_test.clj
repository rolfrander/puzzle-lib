(ns rolfrander.puzzle.map-test
  (:require [clojure.test :refer [deftest is are run-all-tests]]
            [rolfrander.puzzle.map :refer :all]))

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
    :hex-ew-str [1 3] #{[0 1] [0 0] [1 1] [1 2] [0 2]}))

(deftest neighbours-4-test
  (are [origin result] (= result (into #{} ((neighbours-fn :sq-4 :infinite) origin)))
    [0 0] #{[-1 0] [1 0] [0 -1] [0 1]}
    [3 4] #{[2 4] [4 4] [3 3] [3 5]}))

(deftest neighbours-4-clip-test
  (are [origin result] (= result (into #{} ((neighbours-fn :sq-4 :ignore :max-dim [4 4]) origin)))
    [3 4] #{[2 4] [4 4] [3 3]}
    [0 0] #{[1 0] [0 1]}))

(deftest neighbours-3d-test
  (are [origin border result] (= result (into #{} ((neighbours-fn :sq-4 border :dimensions 3 :max-dim [4 5 6]) origin)))
    [3 4 5] :infinite #{[2 4 5] [4 4 5] [3 3 5] [3 5 5] [3 4 4] [3 4 6]}
    [3 4 5] :wrap     #{[2 4 5] [0 4 5] [3 3 5] [3 0 5] [3 4 4] [3 4 0]}
    [4 5 6] :ignore   #{[3 5 6] [4 4 6] [4 5 5]}))

(deftest neighbours-3d-diag-test
  (are [origin result] (= result (into #{} ((neighbours-fn :sq-8 :infinite :dimensions 3) origin)))
    [3 4 5]  #{[2 5 4] [3 5 4] [4 5 4] [2 5 5] [3 5 5] [4 5 5] [2 5 6] [3 5 6] [4 5 6]
               [2 4 4] [3 4 4] [4 4 4] [2 4 5]       [4 4 5] [2 4 6] [3 4 6] [4 4 6]
               [2 3 4] [3 3 4] [4 3 4] [2 3 5] [3 3 5] [4 3 5] [2 3 6] [3 3 6] [4 3 6]}))

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
    [0 0] "sw" :hex-ns-str :infinite nil [-1 0]))

(run-all-tests #"puzzle-lib.test")
