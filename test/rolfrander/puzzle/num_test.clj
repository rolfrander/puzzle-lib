(ns rolfrander.puzzle.num-test
  (:require [clojure.test :refer [deftest is are run-all-tests]]
            [rolfrander.puzzle.num :refer :all]))

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
  (let [is-prime? (get-prime-sieve 2000)]
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
      (* 17 23))))

(deftest gcd-test
  (are [a b res] (and (= res (gcd a b)) (= res (gcd b a)))
    (* 13 17) (* 17 23) 17
    (* 3 5 107) (* 7 11 107) 107
    (* 3 41 59) (* 5 41 43) 41
    (* 2 3 41 59) (* 2 5 41 43) (* 2 41)))

(deftest count-bits-test
  (are [input output] (= output (count-bits input))
    1 1
    2 1
    4 1
    128 1
    15 4
    127 7))

(run-all-tests #"puzzle-lib.test")