
(defn aux-pascal
  [lst]
  (if (< (count lst) 2)
      ()
      (cons (+ (first lst) (second lst))
            (aux-pascal (rest lst)))))

(defn pascal
  "Returns a list with all the elements of row n of Pascal's triangle.
  Rows are numbered starting at zero."
  [n]
  (if (zero? n)
      '(1)
       (concat '(1)  (aux-pascal (pascal (dec n))) '(1))))

(defn convert
  [n]
  (->> (pascal n)
  (map str)
  (interpose " ")
  (apply str)))

(defn pascals
  [x]
  (loop [n 0
         y ""]
    (if(= n x)
     ""
     (recur (inc n) (println (convert n))))))

(def a (read-line))
(println (pascals (Integer/parseInt a)))
