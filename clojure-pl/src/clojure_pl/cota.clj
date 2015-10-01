(ns clojure-pl.cota
  (:require [clojure.test :refer :all]))

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