(ns clojure-pl.cps_test
  (:require [acolfut.sweet :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.cps :refer :all]))

(deftest simple-cases
  (testing "pyth theorem"
    (testing "without cps"
      (is= (pyth-theorem-normal 1 2) 5))
    (testing "with cps"
      (is= (pyth-theorem-cps 1 2 identity))))
  (testing "fibonacci"
    (testing "without cps"
      (is= (fibonacci-normal 5) 5)
      (is= (fibonacci-cps 5 identity) 5)))
  (testing "mk cps"
    (testing "factorial"
      (is= (factorial 10) 3628800)
      (is= (triangular 10) 55))))
