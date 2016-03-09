
;21 nth element
(fn f [s n]
  (first (drop n s)))
;34
(fn
  [a b]
  (loop [result ()
         x a]
    (if(> x b)
      (reverse (rest result))
      (if(<= a x b)
        (recur (cons x result) (inc x))))))
;38

(fn
  [& args]
  (last
  (sort  args)))

;166

(fn
  [f a b]
  (if (= a b)
    :eq
    (if (f a b)
    :gt
    :lt)))
;81
(fn
  [set1 set2]
  (set
  (for [x set1 y set2
      :when (= x y)] x)))


;88


;156
