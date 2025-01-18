(ns rolfrander.puzzle.num)

(defn sign 
  "returns 0, -1 or 1 if x is equal to, less than or greater than zero"
  [x] (cond (= x 0) 0
            (> x 0) 1
            :else -1))

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
