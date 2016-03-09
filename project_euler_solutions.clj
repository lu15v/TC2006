;----------------------------------------------------------
; Activity: Project Euler
; Date: Marzo 3, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

;Euler username --> lu15v
;problems solved (13)--> 3, 4, 6, 7, 9, 10, 14, 15, 16, 20, 25, 29, 48

(use 'clojure.test)

(defn prime?
  [n]
  (empty? (filter #(= 0 (mod n  %)) (range 2 n))))

(def prime-certainty 16)

(defn prime
  [x]
  (.isProbablePrime (BigInteger/valueOf x) prime-certainty))

(defn  pow
  "Raises b to the power e."
    [b e]
    (if(zero? e)
      1N
      (* b (pow b (dec e)))))

;-----------------(problem 3)--------------------------
;auxiliar functions used: prime
;result: 6857

(defn problem3
    [n]
    (let [primes (filter prime (range 1 (inc(int(Math/sqrt n)))))]
    (last(filter #(zero? (mod n %)) primes))))

;------------------------------------------------------

(defn palindrome?
  [n]
  (let [reversed (apply str (reverse (str n)))
        strn (str n)]
    (if(= reversed strn)
        true
        false)))

;-----------------(problem 4)--------------------------
;auxiliar functions used: palindrome?
;result: 906609

(def problem4
  (reduce max (filter #(palindrome? %)
      (for [x (range 100 (inc 999))
            y (range 100 (inc 999))]
      (* x y)))))

;------------------------------------------------------

;-----------------(problem 6)--------------------------
;auxiliar functions used: none
;result: 2.516415E7

(defn problem6
    [n]
    (let [natNum (range 1 (inc n))
          powsum (reduce +(map #(Math/pow % 2)natNum))
          sumpow (Math/pow (reduce + natNum) 2)]
          (- sumpow powsum)))

;------------------------------------------------------


;-----------------(problem 7)--------------------------
;auxiliar functions used: prime?
;result: 104743

(defn problem7
  [n]
  (->> (take n(filter #(prime? %)  (iterate inc 2)))
       (last)))

;------------------------------------------------------


;-----------------(problem 9)--------------------------
;auxiliar functions used: none
;result: 31875000

(defn problem9
  [x]
  (reduce *(first
  (filter (fn [[a b c]] (= (* c c) (+ (* a a) (* b b))))
  (for [a (take 300 (iterate #(inc %) 1))
        b (take 500 (iterate #(inc %) 1))
        :let [c (- x a b)]
        :when #(x = (+ a b c))] [a b c])))))

;------------------------------------------------------


;-----------------(problem 10)--------------------------
;auxiliar functions used: none
;result: 142913828922

(defn problem10
  [n]
  (reduce +'
      (filter prime (range 1 n))))

;------------------------------------------------------

(defn collatz
    [n]
    (loop [n n
           result ()]
    (if (= 1 n)
        (count(cons n result))
        (if(even? n)
           (recur (/ n 2) (cons n result))
           (recur (inc(* 3 n)) (cons n result))))))

;-----------------(problem 14)--------------------------
;auxiliar functions used: collatz
;result: 837799

(defn problem14
  [n]
  (->>
  (take (dec n) (iterate #(+ 1 %) 2))
  (map collatz )
  (map-indexed (fn [i l1] (vector (inc i) l1)))
  (sort-by second)
  (last)
  (first)
  (inc)))

;------------------------------------------------------

(defn factorial [x]
  (loop [n x
        f 1N]
      (if (= n 1N)
        f
        (recur (dec n) (* f n)))))

(defn totalcombinations
  [n k]
  (if(zero? n)
    0
    (/ (factorial n) (* (factorial(- n k)) (factorial k)))))

;-----------------(problem 15)--------------------------
;auxiliar functions used: totalcombinations
;result: 137846528820

(defn problem15
  [n]
  (totalcombinations (+ n n) n))

;------------------------------------------------------

(defn digits-of-number
  [n]
  (->>
    (seq(str n))
    (map #(Character/digit % 10))))

;-----------------(problem 16)--------------------------
;auxiliar functions used: digits-of-number
;result: 1366

(defn problem16
  [n]
  (->> (pow 2 n)
       (digits-of-number)
       (reduce +)))
;------------------------------------------------------

;-----------------(problem 20)--------------------------
;auxiliar functions used: digits-of-number & factorial
;result: 648

(defn problem20
  [n]
  (->>
  (digits-of-number (factorial n))
  (reduce +)))

;------------------------------------------------------

;-----------------(problem 25)-------------------------
;auxiliar functions used: pow
;result: 4782

(def problem25
  (->>
  (iterate
  (fn [[a  b]] [b (+ a b)])
      [1N 1N])
  (map first)
  (map-indexed (fn [i l1] (vector (inc i) l1)))
  (drop-while (fn [[a b]] (< b (pow 10N 999N))))
  (first)
  (first)))

;------------------------------------------------------

(defn eraseduplicates
  [lst]
  (loop [lst lst
          result ()]
    (cond
      (empty? lst) (reverse result)
      (=(first lst) (second lst)) (recur (rest lst) result)
      :else (recur  (rest lst) (cons (first lst) result)))))

;-----------------(problem 29)-------------------------
;auxiliar functions used: pow
;result: 9183

(defn problem29
  [x]
    (count
    (eraseduplicates (sort (for [a (take (- x 1) (iterate #(inc %) 2))
          b (take (- x 1) (iterate #(inc %) 2))](pow a b))))))

;------------------------------------------------------

  (defn take-last-n
    [n lst]
    (let [size (count lst)
          index (- size n)]
      (drop index lst)))

;-----------------(problem 48)-------------------------
;auxiliar functions used: digits-of-number & take-last-n
;result: '(9 1 1 0 8 4 6 7 0 0)


(defn problem48
  [n]
  (->>
  (range 1N (inc n))
  (map #(pow % %))
  (reduce +)
  (digits-of-number)
  (take-last-n 10)))


;----------------(Unit Tests)--------------------------

(deftest test-problem3
  (is (= 6857 (problem3 600851475143))))

(deftest test-problem4
  (is (= 906609 problem4)))

(deftest test-problem6
  (is (= 2.516415E7 (problem6 100))))

(deftest test-problem7
  (is (= 104743 (problem7 10001))))

(deftest test-problem9
  (is (= 31875000 (problem9 1000))))

(deftest test-problem10
  (is (= 142913828922 (problem10 2000000))))

(deftest test-problem14
  (is (= 837799 (problem14 1000000))))

(deftest test-problem15
  (is (= 137846528820 (problem15 20))))

(deftest test-problem16
  (is (= 1366 (problem16 1000))))

(deftest test-problem20
  (is (= 648 (problem20 100))))

(deftest test-problem25
  (is (= 4782 problem25 )))

(deftest test-problem29
  (is (= 9183 (problem29 100))))

(deftest test-problem48
  (is (= '(9 1 1 0 8 4 6 7 0 0) (problem48 1000))))

(run-tests)
