(ns clojure-pl.cota
  (:require [acolfut.sweet :refer :all]))

(def car first)
(def cdr rest)
(def cadr #(car (cdr %)))
(def cddr #(cdr (cdr %)))
(def caadr #(car (car (cdr %))))
(def caddr #(car (cdr (cdr %))))
(def cdadr #(cdr (cadr %)))
(def cadddr #(car (cdr (cddr %))))

(defmacro is= [& body]
  `(is (= ~@body)))

(def boolean? #(or (true? %) (false? %)))

(defn assq
  "Similar to Scheme assq, xs must be a List of Pairs"
  [k xs]
  (loop [xs xs]
    (if (or (nil? xs)
            (empty? xs))
      nil
      (let [x   (first xs)
            lhs (first x)]
        (if (= k lhs)
          (second x)
          (recur (rest xs)))))))