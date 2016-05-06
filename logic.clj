;----------------------------------------------------------
; Activity: MiniKanren
; Date: May 2, 2016.
; Authors:
;          A01371743 Luis E. Ballinas Aguilar
;----------------------------------------------------------

(use '[clojure.core.logic :rename {is logic-is}])
(use 'clojure.test)

(defn removeo
    [x lst result]
      (conde
          [(fresh [h] (conso h result lst) (== h x))]
          [(fresh [h t y] (conso h t lst) (!= h x)  (removeo x t y) (appendo [h] y result))]))

; (defn even-sizeo
;   [lst]
;   (fresh [h t]
;     (conde
;       [(== lst [])]
;       [(conso h t lst) (odd-sizeo t) succeed])))
;
;
; (defn odd-sizeo
;   [lst]
;   (fresh [h t]
;     (conde
;       [(!= lst [])]
;       [(conso h t lst) (even-sizeo t)])))
;

(declare odd-sizeo)

(defne even-sizeo
  [lst]
  ([[]])
  ([[h . t]]
    (odd-sizeo t)))

(defne odd-sizeo
  [lst]
  ([[h]])
  ([[h . t]]
  (!= t [])
    (even-sizeo t)))


(defne reverseo
  [lst result]
    ([[] []])
    ([[h . t] result]
        (fresh [x]
            (reverseo t x)
            (appendo x [h] result))))

(defn palindromeo
  [lst]
  (fresh [result]
  (conde
        [(reverseo lst result) (== lst result)])))


(defn rotateo
  [lst result]
  (fresh [h t]
    (conde
      [(!= lst []) (conso h t lst) (appendo t (list h) result)])))

; (def numbers {:zero 0, :one 1, :two 2, :three 3, :four 4, :five 5,
;               :six 6, :seven 7, :eight 8, :nine 9})
;
;
; (defn get-key
;   [v map]
;   (cond
;     (empty? map) ()
;     (= (second (first map)) v) (first (first map))
;     :else (get-key v (rest map))))
;
;
; (defn converto
;   [d k]
;   (conde
;     [(== d (get numbers k))]
;     [(== k (get-key d numbers))]))



; (defn translateo
;   [lst result]
;   (fresh [h t k x]
;   (conde
;     [(conso h t lst) (converto h k) (translateo t x) (appendo [k] x result)])))

(defne converto
  [d k]
  ([0 :zero])
  ([1 :one])
  ([2 :two])
  ([3 :three])
  ([4 :four])
  ([5 :five])
  ([6 :six])
  ([7 :seven])
  ([8 :eight])
  ([9 :nine]))

(defne translateo
  [lst result]
  ([[][]])
  ([[h] _]  ;when x q
    (converto [h] result))
  ([[h . t] _]
    (fresh [x k]
       (translateo t x)
       (converto h k)
       (appendo [k] x result))))


(defne splito
  [lst a b]
  ([[] [] []])
  ([[h] [h] []])
  ([[h . t] a b]
    (fresh [ht tt fi si]
      (conso ht tt t)
      (splito tt fi si)
      (appendo [h] fi a)
      (appendo [ht] si b))))




; (defn interleaves
;   [lst lst2]
;   (cond
;     (empty? lst) ()
;     :else (cons (first lst) (cons (first lst2) (interleaves (rest lst) (rest lst2))))))


  ; (defn interleaveo
  ;   [lst lst2 result]
  ;   (fresh [h t h2 t2 y]
  ;   (conde
  ;     [(== [] lst) (== [] lst2) (== [] result)]
  ;     [(conso h t lst) (conso h2 t2 lst2) (interleaveo t t2 y) (appendo [h] [h2] result)])))

; (defn splito
;   [lst a b]
;   (fresh [result]
;   (conde
;     [(appendo a b result) (== lst result)])))

(deftest test-removeo
  (is (= [[:b :c :d :e]]
          (run 1 [q] (removeo :a [:a :b :c :d :e] q))))
  (is (= [[:a :b :d :e]]
         (run 1 [q] (removeo :c [:a :b :c :d :e] q))))
  (is (= [:d]
         (run 1 [q] (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (is (= []
         (run 1 [q] (removeo :x [:a :b :c :d :e] q))))
  (is (= [[:x :a :b :c :d :e]
          [:a :x :b :c :d :e]
          [:a :b :x :c :d :e]
          [:a :b :c :x :d :e]
          [:a :b :c :d :x :e]
          [:a :b :c :d :e :x]]
         (run 6 [q] (removeo :x q [:a :b :c :d :e]))))
  (is (= [[:a [:b :c :d :e]]
          [:b [:a :c :d :e]]
          [:c [:a :b :d :e]]
          [:d [:a :b :c :e]]
          [:e [:a :b :c :d]]]
         (run* [q1 q2]
         (removeo q1 [:a :b :c :d :e] q2)))))

(deftest test-even-sizeo-odd-sizeo
  (is (= [:yes]
         (run 1 [q] (even-sizeo []) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (odd-sizeo [:x]) (== q :yes))))
  (is (= []
         (run 1 [q] (even-sizeo [:x]) (== q :yes))))
  (is (= []
         (run 1 [q] (odd-sizeo []) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (even-sizeo [:a :b :c :d :e :f]) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (odd-sizeo [:a :b :c :d :e]) (== q :yes))))
  (is (= '[[]
           [_0 _1]
           [_0 _1 _2 _3]
           [_0 _1 _2 _3 _4 _5]
           [_0 _1 _2 _3 _4 _5 _6 _7]]
         (run 5 [q] (even-sizeo q))))
  (is (= '[[_0]
           [_0 _1 _2]
           [_0 _1 _2 _3 _4]
           [_0 _1 _2 _3 _4 _5 _6]
           [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
         (run 5 [q] (odd-sizeo q)))))


(deftest test-palindromeo
  (is (= [:yes]
         (run 1 [q] (palindromeo []) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (palindromeo [:a]) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (palindromeo [:a :b :c :b :a]) (== q :yes))))
  (is (= []
         (run 1 [q] (palindromeo [:a :b :c :d]) (== q :yes))))
  (is (= '[[]
           [_0]
           [_0 _0]
           [_0 _1 _0]
           [_0 _1 _1 _0]
           [_0 _1 _2 _1 _0]
           [_0 _1 _2 _2 _1 _0]]
         (run 7 [q] (palindromeo q)))))

(deftest test-rotateo
   (is (= [:yes]
          (run 1 [q]
            (rotateo [:a :b :c :d :e]
                     [:b :c :d :e :a])
            (== q :yes))))
   (is (= []
          (run 1 [q]
            (rotateo [:a :b :c :d :e]
                     [:a :b :c :d :e])
            (== q :yes))))
   (is (= []
          (run 1 [q] (rotateo [] q))))
   (is (= [[:a]]
          (run 1 [q] (rotateo [:a] q))))
   (is (= [[:b :c :d :e :a]]
          (run 1 [q] (rotateo [:a :b :c :d :e] q))))
   (is (= [[:e :a :b :c :d]]
          (run 1 [q] (rotateo q [:a :b :c :d :e]))))
   (is (= '[[[_0] [_0]]
           [[_0 _1] [_1 _0]]
           [[_0 _1 _2] [_1 _2 _0]]
           [[_0 _1 _2 _3] [_1 _2 _3 _0]]
           [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
           [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
           [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
           (run 7 [q1 q2] (rotateo q1 q2)))))



(deftest test-converto
  (is (= [:yes]
         (run 1 [q] (converto 0 :zero) (== q :yes))))
  (is (= [:yes]
         (run 1 [q]
           (converto 0 :zero)
           (converto 1 :one)
           (converto 2 :two)
           (converto 3 :three)
           (converto 4 :four)
           (converto 5 :five)
           (converto 6 :six)
           (converto 7 :seven)
           (converto 8 :eight)
           (converto 9 :nine)
           (== q :yes))))
  (is (= []
         (run 1 [q] (converto 2 :one) (== q :yes))))
  (is (= []
         (run 1 [q] (converto 12 :twelve) (== q :yes))))
  (is (= [7]
         (run 1 [q] (converto q :seven))))
  (is (= [:seven]
         (run 1 [q] (converto 7 q))))
  (is (= [[1 :two 3]]
         (run 1 [q1 q2 q3]
           (converto q1 :one)
           (converto 2 q2)
           (converto q3 :three)))))

(deftest test-translateo
   (is (= [:yes]
          (run 1 [q] (translateo [1 2 3] [:one :two :three]) (== q :yes))))
   (is (= []
          (run 1 [q] (translateo [1 2 3] [:one :two :four]) (== q :yes))))
   (is (= [:three]
          (run 1 [q] (translateo [1 2 3] [:one :two q]))))
   (is (= [[:four :five :six :seven :eight :nine]]
          (run 1 [q] (translateo [4 5 6 7 8 9] q))))
   (is (= [[1 2 0]]
          (run 1 [q] (translateo q [:one :two :zero]))))
   (is (= [[[] []]]
          (run 1 [q1 q2] (translateo q1 q2)))))

(deftest test-translateo
  (is (= [:yes]
         (run 1 [q] (translateo [1 2 3] [:one :two :three]) (== q :yes))))
  (is (= []
         (run 1 [q] (translateo [1 2 3] [:one :two :four]) (== q :yes))))
  (is (= [:three]
         (run 1 [q] (translateo [1 2 3] [:one :two q]))))
  (is (= [[:four :five :six :seven :eight :nine]]
         (run 1 [q] (translateo [4 5 6 7 8 9] q))))
  (is (= [[1 2 0]]
         (run 1 [q] (translateo q [:one :two :zero]))))
  (is (= [[[] []]]
         (run 1 [q1 q2] (translateo q1 q2)))))


(deftest test-splito
 (is (= [:yes]
        (run 1 [q] (splito [] [] []) (== q :yes))))
 (is (= [:yes]
        (run 1 [q] (splito [:a] [:a] []) (== q :yes))))
 (is (= [:yes]
        (run 1 [q] (splito [:a :b] [:a] [:b]) (== q :yes))))
 (is (= [:yes]
        (run 1 [q]
          (splito [:a :b :c :d :e :f]
                  [:a :c :e]
                  [:b :d :f])
          (== q :yes))))
 (is (= [:yes]
        (run 1 [q]
          (splito [:a :b :c :d :e :f :g]
                  [:a :c :e :g]
                  [:b :d :f])
          (== q :yes))))
 (is (= [[[:a :c :e] [:b :d :f]]]
        (run 1 [q1 q2] (splito [:a :b :c :d :e :f] q1 q2))))
 (is (= [[:a :b :c :d :e :f :g]]
        (run 1 [q] (splito q [:a :c :e :g] [:b :d :f]))))
 (is (= '[[[] [] []]
          [[_0] [_0] []]
          [[_0 _1] [_0] [_1]]
          [[_0 _1 _2] [_0 _2] [_1]]
          [[_0 _1 _2 _3] [_0 _2] [_1 _3]]
          [[_0 _1 _2 _3 _4] [_0 _2 _4] [_1 _3]]
          [[_0 _1 _2 _3 _4 _5] [_0 _2 _4] [_1 _3 _5]]]
        (run 7 [q1 q2 q3] (splito q1 q2 q3)))))

(run-tests)
