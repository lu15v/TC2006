(defn rotate
  [string]
  (let [n (count string)]
    (->> (range 1 (inc n))
         (map #(take n (drop % (cycle string))))
         (map #(reduce str %))
         (clojure.string/join " "))))

(doseq [line (rest (line-seq (java.io.BufferedReader. *in*)))]
  (println (rotate (seq line))))
