;----------------------------------------------------------
; Problem 83: A Half-Truth
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)



(def problem83
  (fn [& args]
    (if(or(every? true? args)(every? false? args))
      false
      (if (not(not-any? true? args))
        true
        false))))



(deftest test-problem83
(is (= false (problem83 false false)))
(is (= true (problem83 true false)))
(is (= false (problem83 true)))
(is (= true (problem83 false true false)))
(is (= false (problem83 true true true)))
(is (= true (problem83 true true true false))))

(run-tests)
