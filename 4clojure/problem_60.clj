;----------------------------------------------------------
; Problem 60: Sequence Reductions
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)

(def problem60
    (fn ([f x]
      (lazy-seq
      (problem60 f (first x) (rest x))))
    ([f x y]
      (lazy-seq
      (cons x
        (if (empty? y)
          nil
          (problem60 f (f x (first y)) (rest y))))))))



(deftest test-problem60
  (is(= (take 5 (problem60 + (range))) [0 1 3 6 10]))
  (is(= (problem60 conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is(= (last (problem60 * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

(run-tests)
