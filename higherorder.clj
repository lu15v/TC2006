;----------------------------------------------------------
; Activity: Higher-Order Functions
; Date: February 11, 2016.
; Authors:
;          A01371743 Luis E. Ballinas Aguilar
;----------------------------------------------------------

(use 'clojure.test)

(defn my-map-indexed
    " It returns a list consisting of the result of applying f to
    0 and the first item of lst, followed by applying f to 1 and the
    second item in lst, and so on until lst is exhausted. "
    [f lst]
    (loop [l1 lst
           result ()
           x  0]
          (if (empty?  l1)
             (reverse result)
              (recur  (rest l1) (cons  (f x (first l1))result)(inc x)))))

(defn my-drop-while
    "akes two arguments: a function f and a list lst. It returns a list
    of items from lst dropping the initial items that evaluate to true
    when passed to f. Once a false value is encountered, the rest of the
    list is returned. Function f should accept one argument. Do not use the
    predefined drop-while function."
    [f lst]
    (loop [result ()
           l1 lst]
          (if (empty? l1)
              result
              (if (f (first l1))
                (recur () (rest l1))
                (concat result l1)))))


(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))



(defn bisection
     "is a root-finding algorithm which works by repeatedly dividing
    an interval in half and then selecting the subinterval in which
    the root exists"
    [a b f]
    (loop [a a
           b b
           c (/(+ a b) 2.0)]
          (if (< (Math/abs (f c)) 1.0E-15)
              c
              (if (<(* (f a) (f c)) 0)
                  (recur a c (/(+ a b) 2.0))
                  (recur c b (/(+ a b) 2.0))))))



 (defn deriv
     "returns a new function that takes x as argument, and which represents
     the derivate of f given a certain value for h."
     [f h]
     (fn [x]
         (/(- (f (+ x h)) (f x)) h)))

(defn integral
    "Calculates the Simpson's Rule"
    [a b n f]
    (loop [k 0
           aprox 0]
       (let [ h (/(- b a)n)
              multEven 2
              multOdd  4]
          (if (= k n)
              (/(*( + aprox (f (+ a (* k h))))h)3)
              (if(zero? k)
                (recur (inc k) (+ aprox (f (+ a (* k h)))))
                (if(even? k)
                    (recur (inc k)  (+ aprox (* (f (+ a (* k h))) multEven)))
                    (recur (inc k)  (+ aprox (* (f (+ a (* k h))) multOdd)))))))))



(defn drops
    [lst n]
    (loop [l1 lst
          result ()
           k  1]
        (if (empty? l1)
            result
            (if (not= k n)
              (recur (rest l1) (cons (first l1) result) (inc k))
              (recur (rest l1) result 1)))))


(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))


(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))


(deftest test-bisection
  (is (aprox= 0.0001 3.0 (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 -4.0 (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 Math/PI (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 (* 2 Math/PI) (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 1.618033988749895
                     (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001 -0.6180339887498948
                     (bisection -10 1 (fn [x] (- (* x x) x 1))))))


(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
           (fn [x]
             (integral 3 4 10
               (fn [y]
                 (* x y))))))))

(run-tests)
