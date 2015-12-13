(ns clojure-pl.cps)

(defn pyth-theorem-normal
  [a b]
  (+ (* a a) (* b b)))

(defn *-cps
  [x y k]
  (k (* x y)))

(defn +-cps
  [x y k]
  (k (+ x y)))

(defn pyth-theorem-cps
  [a b k]
  (*-cps a a (fn [a2]
               (*-cps b b (fn [b2]
                            (+-cps a2 b2 k))))))

(defn fibonacci-normal
  [n]
  (if (<= n 1)
    n
    (+ (fibonacci-normal (- n 1))
       (fibonacci-normal (- n 2)))))

(defn fibonacci-cps
  [n k]
  (letfn [(cont [n1]
            (fibonacci-cps (- n 2) (fn [n2]
                                     (k (+ n1 n2)))))]
    (if (<= n 1)
      (k n)
      (recur (- n 1) cont))))

(defn mk-cps
  [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k (kont v n)))]
         (if (accept? n)
           (k end-value)
           (recur (dec n) cont))))
      n kend)))

(def factorial (mk-cps zero? 1 identity #(* %1 %2)))
(def triangular (mk-cps zero? 1 dec #(+ %1 %2)))
