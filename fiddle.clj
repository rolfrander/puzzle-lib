
; https://4clojure.oxal.org/#/problem/92
(fn [in]
  (let [roman (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])]
    (apply + (reduce (fn [[total prev] cur]
                       (if (< prev cur)
                         [(- total prev) cur]
                         [(+ total prev) cur])) [0 0] (map roman in)))))

; https://4clojure.oxal.org/#/problem/104
(fn [in]
  (let [roman-map (map vector
                       [1000 900 500 400  100  90   50  40   10  9    5   4    1]
                       ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])
        to-roman (fn rom [i [[r l] & rest-romans :as romans]]
                   (cond (= i 0) nil
                         (>= i r) (cons l (rom (- i r) romans))
                         :else (rom i rest-romans)))]
    (apply str (to-roman in roman-map))))

(eval '(let [y (quote (fn [x] (str (let [y (quote (fn [x] (str x)))] x))))] ((eval y) y)))

; https://4clojure.oxal.org/#/problem/135 infix calc

((fn rec
   ([num] num)
   ([num1 oper num2 & tail]
    (apply rec (oper num1 num2) tail)))
 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)

(defn run-test [f cases]
  (doseq [[expected & params] cases]
    (if (= expected (apply f params))
      (println "OK  " expected params)
      (println "FAIL" expected params))))

;https://4clojure.oxal.org/#/problem/106
(let [f (fn [a b]
          (loop [vals #{a}
                 cnt 1]
            (if (vals b)
              cnt
              (recur (into #{} (for [x vals
                                     y (cond-> [(partial * 2)
                                                (partial + 2)]
                                         (even? x) (conj #(/ % 2)))]
                                 (y x)))
                     (inc cnt)))))]
  (run-test f [[1 1 1]
               [3 3 12]
               [3 12 3]
               [3 5 9]
               [9 9 2]
               [5 9 12]]))