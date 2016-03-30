(defn fibo
  [x]
  (if(= 1 x)
    0
    (if(= 2 x)
      1
      (+ (fibo (- x 1)) (fibo (- x 2))))))

(def a (read-line))


(println (fibo (Integer/parseInt a)))
