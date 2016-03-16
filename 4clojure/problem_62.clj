;----------------------------------------------------------
; Problem 62: Re-implement Iterate
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)

(def problem62
  (fn [fx n]
    (lazy-seq (cons n (problem62 fx (fx n))))))





(deftest test-problem62
(is (= (take 5 (problem62 #(* 2 %) 1)) [1 2 4 8 16]))
(is (= (take 100 (problem62 inc 0)) (take 100 (range))))
(is (= (take 9 (problem62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(run-tests)
