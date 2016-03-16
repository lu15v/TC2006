;----------------------------------------------------------
; Problem 61: Map Construction
; Date: March 17, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)

(def problem61
  #(apply assoc {} (interleave %1 %2))
)

(deftest test-problem61
  (is (= (problem61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (problem61 [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (problem61 [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(run-tests)
