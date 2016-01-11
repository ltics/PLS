(ns clojure-pl.mukr-test
  (:require [acolfut.sweet :refer :all]
            [clojure-pl.cota :refer :all :exclude [car cdr]]
            [clojure-pl.mukr.mukr :refer :all]))

(deftest test-cons
  (let [lst  (->Cons 1 (->Cons 1 nil))
        pair (->Cons 1 1)]
    (testing "tostring"
      ;;a scheme like list construct use cons
      (is= (str lst) "(1 1)")
      ;;a scheme like pair construct use cons
      (is= (str pair) "(1 . 1)"))
    (testing "seqable"
      (is= (seq lst) '(1 1))
      (is (nil? (seq pair))))))
