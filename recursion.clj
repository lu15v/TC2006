;----------------------------------------------------------
; Activity: Recursive Functions, Part I
; Date: January 28, 2016.
; Authors:
;          A01371743 Luis E. Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)

(defn my-count
    [x]
    (if(empty? x)
        0
        (+ 1 (my-count(rest x)))))

(defn add-list
    [x]
    (if (empty? x)
        0
        (+ (first x) (add-list(rest x)))))


(defn member?
    [x lst]
    (cond
        (= x (first lst)) true
        (empty? lst) false
        :else (member? x (rest lst))))


(defn list-of-symbols?
     [lst]
     (if (empty? lst)
         true
         (if (not(symbol? (first lst)))
             false
             (list-of-symbols? (rest lst)))))


(defn my-last
      [lst]
      (cond
          (empty? lst) nil
          (= nil (second lst)) (first lst)
          :else (my-last (rest lst))))


(defn cons-end
    [x lst]
    (cond
        (empty? lst) (cons x lst)
        :else (cons (first lst) (cons-end x (rest lst)))))


(defn my-reverse
  [lst]
  (loop [x lst
         result ()]
    (if (empty? x)
      result
      (recur (rest x)
             (cons (first x) result)))))


(defn my-butlast [lst]
  (if (empty? lst)
    nil
    (loop [result [] l lst]
      (if (= (second l) nil)
        (my-reverse result)
        (recur (cons (first l) result) (rest l))))))


(defn my-concat
  [lst1 lst2]
  (if (empty? lst1)
    lst2
    (if (empty? lst2)
      lst1
      (loop [l1 lst1
             l2 lst2]
        (if (empty? l2)
          l1
          (recur (cons-end (first l2) l1) (rest l2)))))))


(defn deep-reverse
  [lst]
  (cond
   (empty? lst) ()
   (list? (first lst)) (cons-end (deep-reverse (first lst)) (deep-reverse(rest lst)))
   :else  (cons-end(first lst) (deep-reverse (rest lst)))))



(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))

(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1)) (deep-reverse '((1 2) 3 (4 (5 6)))))))

(run-tests)
