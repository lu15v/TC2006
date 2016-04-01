;----------------------------------------------------------
; Problem 144: Oscilrate
; Date: April 1, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)


(defn problem144
  [x & args]
  (cons x
    (lazy-seq
      (apply problem144
          (cons ((first args) x)
            (concat (rest args) (take 1 args)))))))





(deftest test-problem144
  (is(= (take 3 (problem144 3.14 int double)) [3.14 3 3.0]))
  (is(= (take 5 (problem144 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
  (is(= (take 12 (problem144 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))
(run-tests)
