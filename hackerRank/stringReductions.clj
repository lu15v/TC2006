(defn reduction
  [string]
  (->> (seq string)
  (distinct)
  (apply str)))

(def a (read-line))

(println (reduction  a))
