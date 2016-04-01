;----------------------------------------------------------
; Problem 70: Word Sorting
; Date: April 1, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------


(use '[clojure.string :as str])
(use 'clojure.test)

(defn problem70
  [s]
  (let [cleaned (first (str/split s #"[.!]+"))]
  (->>(str/split cleaned #"\s+")
      (group-by clojure.string/lower-case)
      (sort)
      (map #(first (second %))))))






(deftest test-problem70

  (is(= (problem70  "Have a nice day.")
     ["a" "day" "Have" "nice"]))
  (is(= (problem70  "Clojure is a fun language!")
     ["a" "Clojure" "fun" "is" "language"]))
  (is(= (problem70  "Fools fall for foolish follies.")
     ["fall" "follies" "foolish" "Fools" "for"])))

(run-tests)
