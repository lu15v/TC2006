;----------------------------------------------------------
; Problem 29: Get the Caps
; Date: March 31, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------
(use 'clojure.test)
(require '[clojure.string :as str] )


(defn problem29
  [str]
      (str/join
      (filter #(Character/isUpperCase %) (seq str))))


(deftest test-problem29
  (is(= (problem29 "HeLlO, WoRlD!") "HLOWRD"))
  (is(empty? (problem29 "nothing")))
  (is(= (problem29 "$#A(*&987Zf") "AZ")))


(run-tests)
