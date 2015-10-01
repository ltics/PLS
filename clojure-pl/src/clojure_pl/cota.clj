(ns clojure-pl.cota
  (:require [clojure.test :refer :all]))

(defmacro is= [& body]
  `(is (= ~@body)))

(def boolean? #(or (true? %) (false? %)))
