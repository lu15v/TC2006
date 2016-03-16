;----------------------------------------------------------
; Problem 95: To Tree, or not to Tree
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)

(def problem95
 (fn
 [t]
 (cond
  (nil? t) true
  (or (not (coll? t)) (not= (count t) 3)) false
  (and (= 3 (count t)) (problem95 (nth t 1)) (problem95 (nth t 2))) true
  :else false)))

(deftest test-problem95
  (is(=(problem95 '(:a (:b nil nil) nil)) true))
  (is(=(problem95 '(:a (:b nil nil))) false))
  (is(=(problem95 [1 nil [2 [3 nil nil] [4 nil nil]]]) true))
  (is(=(problem95 [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false))
  (is(=(problem95 [1 [2 [3 [4 nil nil] nil] nil] nil]) true))
  (is(=(problem95 [1 [2 [3 [4 false nil] nil] nil] nil]) false))
  (is(= (problem95 '(:a nil ())) false)))
(run-tests)
