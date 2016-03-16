;----------------------------------------------------------
; Problem 135: Infix Calculator
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)

(def problem135
   (fn [& args]
     (reduce
       #(if (fn? %1)
            (%1 %2)
            (partial %2 %1))
             args)))

(deftest test-problem135
(is (= 7  (problem135 2 + 5)))
(is (= 42 (problem135 38 + 48 - 2 / 2)))
(is (= 8  (problem135 10 / 2 - 1 * 2)))
(is (= 72 (problem135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(run-tests)
