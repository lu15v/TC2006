
(defn !
  [n]
  (reduce * (range 1N (inc n))))

(defn ex
  [x]
  (let [first2  (+ 1 x)]
  (loop [result ()
        pot 2]
    (if (= pot 10)
      (format "%.4f" (reduce + (cons first2 result)))
      (recur (cons (/ (Math/pow x pot) (! pot)) result)
      (inc pot))))))



(doseq [line (rest (line-seq (java.io.BufferedReader. *in*)))]
  (println (ex (read-string line))))
