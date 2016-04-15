;;; ITESM CEM, April 14, 2016.
;;; Clojure Source File
;;; Activity: Macros
;;; Authors:
;;;          A01371743 Luis E. Ballinas Aguilar
(ns macros)

(defmacro my-or
  "Evaluates its expressions one at a time, from left to right.
  If a form returns a logical true value, it returns that value
  and doesn't evaluate any of the other expressions, otherwise
  it returns the value of the last expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & y]
    `(let [temp# ~x]
      (if-not temp#
        (my-or ~@y)
        temp#))))

(defmacro do-loop
  "Implements the do-loop functionality, if in the condition (last
    argument of the body), specify :while, the body of the loop is
    repeated while the condition holds true, :until, repeat the
    body while the conditon holds false, another expression returns
    nil"
  [& body]
  `(let [condition# ~(first (last body))]
    (do-while condition  ~@body)))


(defmacro do-while [condition & body]
  `(loop []
      ~@(butlast body)
        (if (= condition :while)
            (when ~(last (last body)) (recur))
            (if (= condition :until)
              (when-not ~(last (last body)) (recur))
              nil))))



(defmacro def-pred
  "Takes a name, an arg vector, and a body of one or more expressions,
  and creates two functions, the first one with the specified name and body
  and the second with the prefix 'not-' with the opposite logic of the first one"
  [name vectr & body]
       `(do (defn ~name ~vectr ~@body)
           (defn ~(symbol (str "not-" name)) ~vectr   (not (do ~@body)))))



(defmacro create-function
  ([lst x]
    `(fn [~x] (do ~@lst)))
  ([lst x & args]
    `(fn [~x] (create-function ~lst ~@args))))

(defmacro defn-curry
  "Perfomrs a currying transformation to a function definition. It Takes
  as parameters a name, an args vector and a body of one of more expressions."
  [name vectr & body]
  (let [fst (first vectr)
        res (rest vectr)]
        (cond
          (> (count vectr) 1) `(defn ~name [~fst] (create-function ~body ~@res))
          (= (count vectr) 1) `(defn ~name [~fst] (do ~@body))
          :else `(defn ~name [] (do ~@body)))))
