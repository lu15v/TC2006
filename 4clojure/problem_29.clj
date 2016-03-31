;----------------------------------------------------------
; Problem 29: Get the Caps
; Date: March 31, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)


(defn problem29
  [str]
  (->> (seq str)
       (filter #(Character/isUpperCase %))
       (apply str)))

(deftest test-problem29
  (is(= (problem29 "HeLlO, WoRlD!") "HLOWRD"))
  (is(empty? (problem29 "nothing")))
  (is(= (problem29 "$#A(*&987Zf") "AZ"))))
