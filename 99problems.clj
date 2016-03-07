;-------------------------(1)--------------------------------
(defn my-last
 [lst]
(loop [l1 lst
       result ()]
  (if(empty? l1)
    result
    (if(= (second l1) nil)
      (recur (rest l1) (cons (first l1) result))
      (recur (rest l1) result)))))


(my-last '(1 2 3 4))


;-------------------------(2)--------------------------------

(defn my-but-last
  [lst]
  (loop [l1 lst
         result ()
         size (count l1)]
    (if (empty? l1)
      result
      (if(= size 1)
        (recur (rest l1) (cons (first l1) result) (dec size))
        (recur (rest l1) result (dec size))))))

(my-but-last '(a b c d))

;-------------------------(3)--------------------------------

(defn element-at
  [lst index]
  (loop [l1 lst
         result ()
         count 1]
    (if (= count index)
      ( cons (first l1) result)
      (recur (rest l1) result (inc count)))))


(element-at '(a b c d e) 3)


;-------------------------(4)--------------------------------

(defn my-count
  [lst]
  (loop [l1 lst
         count 0]
    (if (empty? l1)
      count
      (recur (rest l1) (inc count)))))

(my-count '(1 2 3 4))

;-------------------------(5)--------------------------------
(defn my-reverse
  [lst]
  (loop [l1 lst
         result ()]
    (if (empty? l1)
      result
      (recur (rest l1) (cons (first l1) result)))))

(my-reverse '(1 2 4 5 6))

;-------------------------(6)--------------------------------


;-------------------------(7)--------------------------------
(defn my-flatten
  [lst]
   "Takes any nested combination of sequential lists,
   and returns their contents as a single, flat sequence."
  (cond
    (empty? lst) lst
    (list? (first lst)) (concat (my-flatten (first lst)) (my-flatten (rest lst)))
    :else (cons (first lst) (my-flatten (rest lst)))))

;-------------------------(8)--------------------------------

(defn compress
  [lst]
  "removes the repeated elements in a list"
  (cond
    (empty? lst) lst
    (= (first lst) (first(rest lst))) (compress (rest lst))
    :else (cons (first lst) (compress (rest lst)))))

;-------------------------(9)--------------------------------
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

;-------------------------(10)--------------------------------

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

;-------------------------(11)--------------------------------

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

;-------------------------(12)--------------------------------

(defn decode
  [lst]
  "Returns the decoded version of the result of encode-modified"
  (loop [l1 lst
         result ()]
  (if (empty? l1)
       (flatten(reverse result))
      (if (coll? (first l1))
          (recur (rest l1) (cons (repli (first(first l1)) (second(first l1))) result))
          (recur (rest l1) (cons (first l1) result))))))

;-------------------------(14)--------------------------------
(defn dup
  [lst]
  "Retuns a list with every element repeated"
  (loop [l1 lst
         result ()
         fst (first l1)]
    (if(empty? l1)
      (reverse result)
      (recur (rest l1) (cons fst (cons fst result)) (second l1)))))


(dup '(1 2 3 4 5))
;-------------------------(15)--------------------------------

(defn repli
  [n x]
  "Repeats n times x"
  (loop  [result ()
          number n]
  (if (zero? number)
    result
    (recur(cons x result) (dec number)))))


;-------------------------(16)--------------------------------

(defn drop
  [lst n]
  "Retuns a list with the droped element especified on the index"
  (loop [l1 lst
         x 1
         result ()]
    (if(empty? l1)
      (reverse result)
      (if(= n x)
        (recur (rest l1) 1 result)
        (recur (rest l1) (inc x) (cons (first l1) result))))))

(drop '(1 2 3 4 5 6 7 8) 3)

;-------------------------(17)--------------------------------

(defn split
  [lst n]
  "retuns a list splited n elements"
  (loop [l1 lst
         result ()
         firstf ()
         secondf ()
         k 1]
    (if (empty? l1)
      (cons (reverse firstf) (list (reverse secondf)))
      (if (<= k n)
        (recur (rest l1) result (cons (first l1) firstf) secondf (inc k))
        (recur (rest l1) result  firstf (cons (first l1) secondf) (inc k))))))

(split '(1 2 3 4 5 6 7) 3)

;-------------------------(18)--------------------------------
(defn slice
  [lst ii fi]
  "Returns a sliced list with the specified initial index and final index"
  (loop [l1 lst
         result ()
         k 1]
    (if (> k fi)
      (reverse result)
      (if (<=  ii k fi)
        (recur (rest l1) (cons (first l1) result) (inc k))
        (recur (rest l1) result (inc k))))))

(slice '(1 2 3 4 5 6 7) 2 5)

;-------------------------(19)--------------------------------

(defn rotate
  [lst n]
  (loop [l1 lst
         x 1
         result ()
         rotate ()]
    (if (empty? l1)
      (concat (reverse rotate) (reverse result))
      (if (<= x n)
        (recur (rest l1) (inc x) (cons (first l1) result) rotate)
        (recur (rest l1) (inc x) result (cons (first l1) rotate))))))


(rotate '(1 2 3 4 5 6 7) 2)

;-------------------------(20)--------------------------------

(defn remove-at
  [lst n]
  (loop [l1 lst
         x 1
         result ()]
    (if (empty? l1)
      (reverse result)
      (if(= n x)
        (recur (rest l1) (inc x) result)
        (recur (rest l1) (inc x) (cons (first l1) result))))))

(remove-at '(1 2 3 4 5 6 7) 2)

;-------------------------(21)--------------------------------

(defn insert-at
  [lst element index]
  (loop [l1 lst
         x 1
         result ()]
    (if(empty? l1)
      (reverse result)
      (if(= index x)
      (recur (rest l1) (inc x) (cons (first l1) (cons element result)))
      (recur (rest l1) (inc x) (cons (first l1) result))))))



;-------------------------(22)--------------------------------

(defn range
  [a b]
  (loop [result ()
         x 1]
    (if(> x b)
      (reverse result)
      (if(<= a x b)
        (recur (cons x result) (inc x))))))



;-------------------------(23)--------------------------------

(defn rnd-select
  [lst n]
  "Returns a list with n random elements choosed in the input list"
  (loop [x 0
         ran (rand-int (count lst))
         result ()]
    (if(or(empty? lst) (= n x))
     (reverse result)
      (recur  (inc x) (rand-int (count lst)) (cons (nth lst ran) result) ))))




;-------------------------(24)--------------------------------

(defn lotto-select
  [n range]
  (loop [x 0
         result ()]
    (if(= n x)
      result
      (recur (inc x) (cons (rand-int range) result)))))

 ;-------------------------(25)--------------------------------

 (defn rnd-permu
   [lst]
   (loop [l1 lst
          result ()
          ran (+ 1(rand-int (count l1)))]
     (if(empty? l1)
       result
       (recur (remove-at l1 ran) (cons (nth lst ran) result)   (+ 1(rand-int (count l1))) ))))
