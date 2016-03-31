;----------------------------------------------------------
; Problem 49: Split a sequence
; Date: March 31, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)


(defn problem49
  [n vectr]
  (let [fst (vector (vec (first (partition-all n vectr))))]
      (conj  fst (vec (drop n vectr)))))


(deftest test-problem60
  (is(= (problem49 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is(= (problem49 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is(= (problem49 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))
(run-tests)
