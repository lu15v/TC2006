;----------------------------------------------------------
; Activity: Recursive Functions, Part II
; Date: February 4, 2016.
; Authors:
;          A01371743 Luis Eduardo Ballinas Aguilar
;          A01169701 Rodolfo Andres Ramirez Valenzuela
;----------------------------------------------------------

(use 'clojure.test)

(defn my-repeat
  [n x]
  "Repeats n times x"
  (loop  [result ()
          number n]
  (if (zero? number)
    result
    (recur(cons x result) (dec number)))))

(defn invert-pairs
  [vect]
  "Inverts every vector"
  (loop [vect2 vect
         result ()]
    (if (empty? vect2)
      (reverse result)
      (recur (pop vect2) (conj result (vec(reverse (first vect2))))))))


(defn enlist
  [lst]
  "Surrounds in a list every upper-level element of the list it takes as input."
  (loop [l1 lst
         result ()]
  (if(empty? l1)
    (map list (reverse result))
    (recur (pop l1)   (cons (first l1) result) ))))

(defn my-interleave
  [lst lst2]
  "Returns a lazy seq of the first item in each coll, then the second etc."
  (loop [l1 lst
         l2 lst2
         result ()]
     (if(or (empty? l1) (empty? l2))
       (reverse result)
     (recur (pop l1) (pop l2) (cons (first l2) (cons (first l1) result))))))

(defn my-flatten
  [lst]
   "Takes any nested combination of sequential lists,
   and returns their contents as a single, flat sequence."
  (cond
    (empty? lst) lst
    (list? (first lst)) (concat (my-flatten (first lst)) (my-flatten (rest lst)))
    :else (cons (first lst) (my-flatten (rest lst)))))


(defn exchange
  [x1 x2 lst]
  "Returns a list with the same elements as lst, except that
  all occurrences of x1 are replaced by x2 and vice versa"
  (cond
    (empty? lst) lst
    (= x1 (first lst)) (cons x2 (exchange x1 x2 (rest lst)))
    (= x2 (first lst)) (cons x1 (exchange x1 x2 (rest lst)))
    (list? (first lst)) (cons (exchange x1 x2 (first lst)) (exchange x1 x2 (rest lst)))
    :else (cons (first lst) (exchange x1 x2 (rest lst)))))



(defn insert
  [n lst]
  "Returns a new list with the same elements as lst but inserting n in its corresponding place"
  (cond
    (empty? lst) (cons n lst)
    (<= n (first lst)) (cons n (insert (first lst) (rest lst)))
    :else (cons (first lst) (insert n (rest lst)))))




(defn my-sort
  [lst]
  "Returns a new list with the same elements but in ascending order"
  (loop [l1 lst
         result ()]
   (if (empty? l1)
       result
       (recur (rest l1) (insert (first l1) result)))))


(defn binary
  [n]
  "Returns the equivalent binary of n"
  (loop [no n
         result ()]
    (if (zero? no)
        result
        (if(= n 1)
              (recur (dec no) (cons 1 result))
              (recur (quot no 2) (cons (rem no 2) result))))))



(defn prime-factors
  [n]
  "Returns a list with the prime factors of n"
  (loop [x 2
         result ()
         no n]
    (if (or (= (quot no x) 0) (= no 1))
           (reverse result)
           (if(=(rem no x) 0)
            (recur x (cons x result) (quot no x))
            (recur (inc x) result no)))))



(defn compress
  [lst]
  "removes the repeated elements in a list"
  (cond
    (empty? lst) lst
    (= (first lst) (first(rest lst))) (compress (rest lst))
    :else (cons (first lst) (compress (rest lst)))))



(defn pack
  [lst]
  "Returns a list with the same items into a nested list"
  (loop [result ()
         l1 lst
         equals ()]
    (if (empty? l1)
        (reverse result)
        (if(= (first l1) (first (rest l1)))
              (recur result (rest l1)  (cons (first l1) equals))
              (recur (cons (cons (first l1) equals)result) (rest l1) (empty equals))))))


 (defn encode
   [lst]
   "Returns a list of vectors [n e] where n is the times that e appears in the list"
   (loop [result ()
         l1 lst
         size 0]
      (if (empty? l1)
        (reverse result)
        (if(= (first l1) (first (rest l1)))
              (recur result (rest l1) (inc size))
              (recur (cons [(inc size) (first l1)] result) (rest l1) 0)))))


(defn encode-modified
   [lst]
   "Improves the encode method, leaving the single elements without vector"
   (loop [result ()
         l1 lst
         size 1]
      (if (empty? l1)
        (reverse result)
        (if(= (first l1) (first (rest l1)))
              (recur result (rest l1) (inc size))
              (if (not= (first l1) (first (rest l1)))
                  (if(= size 1)
                    (recur (cons (first l1) result) (rest l1) 1)
                    (recur (cons [size (first l1)] result) (rest l1) 1)))))))


(defn decode
  [lst]
  "Returns the decoded version of the result of encode-modified"
  (loop [l1 lst
         result ()]
  (if (empty? l1)
       (flatten(reverse result))
      (if (coll? (first l1))
          (recur (rest l1) (cons (my-repeat (first(first l1)) (second(first l1))) result))
          (recur (rest l1) (cons (first l1) result))))))




(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))


(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))


(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))

(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
         (exchange true 42 '((true) 42 ((cool (42)) (true))))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))



(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))


(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))


(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))


(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))



(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))


(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
