(ns clojure-pl.lc-test
  (:require [acolfut.sweet :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.lc.utlc :refer :all]
            [clojure-pl.lc.utlctc :refer :all]))

(deftest lc-test
  (testing "utlc test cases"
    (is= (interp '(+ 1 2)) 3)
    (is= (interp '(* 2 3)) 6)
    (is= (interp '(* 2 (+ 3 4))) 14)
    (is= (interp '(* (+ 1 2) (+ 3 4))) 21)
    (is= (interp '((lambda (x) (* x 3)) 3)) 9)
    (is= (interp '((lambda (x) (* 2 x)) 3)) 6)
    (is= (interp '(((lambda (x) (lambda (y) (* x y))) 2) 3)) 6)
    ;;if it is dynamic scoping it will be 8 not 6
    (is= (interp '((lambda (y) (((lambda (y) (lambda (x) (* y 2))) 3) 0)) 4)) 6)
    (is= (interp '((lambda (y) (((lambda (y) (lambda (x) (* y x))) 3) 3)) 4)) 9)
    (is= (interp '(1 2)) "no matching clause"))
  (testing "utlctc test cases"
    (is= (analyze 1) {:op  :const
                      :val 1})
    (is= (analyze '(let [a 1] a)) {:op   :let
                                   :name 'a
                                   :init {:op  :const
                                          :val 1}
                                   :body {:op   :local
                                          :name 'a}})
    (is= (analyze '(lambda [a] a)) {:op    :lambda
                                    :param 'a
                                    :body  {:op   :local
                                            :name 'a}})
    (is= (analyze '(let [a (lambda [x] x)] (a 1)))
         {:op   :let
          :name 'a
          :init {:op    :lambda
                 :param 'x
                 :body  {:op   :local
                         :name 'x}}
          :body {:op    :app
                 :rator {:op   :local
                         :name 'a}
                 :rand  {:op  :const
                         :val 1}}})))
