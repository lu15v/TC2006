;----------------------------------------------------------
; Problem 137: Digits an bases
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)


(defn digits-of-number
  [n]
  (->>
    (seq(str n))
    (map #(Character/digit % 10))
    (vec)))

(def problem137
  (fn [no base]
    (loop [no no
          result []]
      (if(= base 10)
        (digits-of-number no)
        (if (< no base)
          (conj result 0)
          (if (< (quot no base) base)
            (vec(cons (quot no base) (cons (rem no base) result)))
            (recur (quot no base) (cons (rem no base)result))))))))

(deftest test-problem137
  (is(= [1 2 3 4 5 0 1] (problem137 1234501 10)))
  (is(= [0] (problem137 0 11)))
  (is(= [1 0 0 1] (problem137 9 2)))
  (is(= [1 0] (let [n (rand-int 100000)](problem137 n n))))
  (is(= [16 18 5 24 15 1] (problem137 Integer/MAX_VALUE 42))))

(run-tests)
