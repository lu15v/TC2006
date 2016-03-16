;----------------------------------------------------------
; Problem 63: Group a Sequence
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)




(def problem63
  (fn
  [f seq]
  (->> (map #(vector (f(first %)) (vec %))
       (partition-by f (sort seq)))
       (into {}))))


(deftest test-problem63
(is(= (problem63 #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
(is(= (problem63 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
(is(= (problem63 count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(run-tests)
