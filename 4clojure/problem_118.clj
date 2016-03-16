;----------------------------------------------------------
; Problem 118: Re-implement Map
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)

(def problem118
  (fn [f x]
    (if(empty? x)
      nil
      (lazy-seq (cons (f (first x)) (problem118 f (rest x)))))))


(deftest test-problem118
(is(= [3 4 5 6 7]
      (problem118 inc [2 3 4 5 6])))

(is(= (repeat 10 nil)
   (problem118 (fn [_] nil) (range 10))))

(is(= [1000000 1000001]
   (->> (problem118 inc (range))
        (drop (dec 1000000))
        (take 2)))))

(run-tests)
