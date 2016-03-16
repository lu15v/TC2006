;----------------------------------------------------------
; Problem 99: Product Digits
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)




(def problem99
   (fn [a b]
       (vec(map #(Character/digit % 10) (seq(str (* a b)))))))



(deftest test-problem99
(is (= (problem99 1 1) [1]))
(is (= (problem99 99 9) [8 9 1]))
(is (= (problem99 999 99) [9 8 9 0 1])))

(run-tests)
