(use 'clojure.test)

(defn add-list
    "returns.."
    [lst]
    (reduce + lst))

(defn  list-of-symbols?
    [lst]
    (every? symbol? lst))

(defn invert-pairs
    ""
    [lst]
    (map (fn [[a b]] [b a]) lst))

(defn enlist
    ""
    [lst]
    (map list lst))

(defn insert
    ""
    [n lst]
    (concat (take-while #(< % n)lst)
            (list n)
            (drop-while #(< % n) lst)))

(defn concat-evens
        [vec1 vec2]
        ((fn [x] (filter even? x))
         (concat vec1 vec2)))


(defn my-reduce
 "Our own implementation of the reduce function"
 [fun init lst]
 (if (empty? lst)
     init
     (fun (first lst) (my-reduce fun init (rest lst)))))
;el segundo corchete indica que estas destructuring
(defn binary
 [n]
(->>  ;threading operators
  [n ()]
  (iterate (fn [[n r]]
            [(quot n 2) (cons (rem n 2) r)]))
  (drop-while (fn [[n _]] (not (zero? n))))
  first
  second))
"
(defn binary
 [n]
 (second
  (first
   (drop-while
    (fn [[n _]] (not (zero? n)))
    (iterate (fn [[n r]]
              [(quot n 2) (cons (rem n 2) r)])
              [n ()])))))
"

(defn pack
  [lst]
    (partition-by identity lst))

(defn compress
  [lst]
  (->>
    (pack lst)
    (map first)))

(defn encode
      [lst]
      (->> (pack lst)
         (map #(vector (count %) (first %)))))



(defn encode-modified
    [lst]
    (->>
        (encode lst)
        (map (fn [[c e]]
                 (if (= c 1)
                 e
                 [c e])))))


(defn decode
    [lst]
    (mapcat (fn [x] (if (vector? x)
                        (repeat (x 0) (x 1))
                        (list x))) lst))


(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))


  (deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

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
