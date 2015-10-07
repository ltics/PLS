(ns clojure-pl.lisp.meta-test
  (:require [acolfut.sweet :refer :all])
  (:require [clojure-pl.cota :refer :all]
            [clojure-pl.lisp.metainterp.meta :refer :all])
  (:refer-clojure :exclude [cond cons let]))

(deftest meta-test
  (testing "define vars"
    (define kalle 4)
    (is= kalle 4)
    (define olle (+ 1 1 1))
    (is= olle (+ 1 1 1)))
  (testing "define functions"
    (define (factorial x) (if (= x 0) 1 (* x (factorial (- x 1)))))
    (is= (factorial (+ 5 5)) 3628800))
  (testing "cond"
    (letfn [(test-fn [x]
                     (cond ((< x 0) (- 0 x)) ((= x 0) 100) (else x)))]
      (is= (test-fn -1) 1)
      (is= (test-fn 0) 100)
      (is= (test-fn 1) 1)))
  (testing "cons"
    (is= (cons 1 2) '(1 2))
    (is= (cons 1 (cons 2 (cons 3 4))) '(1 2 3 4))
    (is= (cons 1 (cons 2 3)) '(1 2 3))
    (is= (cons (cons 1 2) (cons 3 4)) '((1 2) 3 4))
    (is= (cons (- 2 1) (cons 2 (+ 1 1 1))) '(1 2 3))
    (is= (cons "kalle" 2) '("kalle" 2)))
  (testing "append"
    (is= (append (list 1) (list 2)) '(1 2))
    (is= (append '(1 2) '(3 4)) '(1 2 3 4))
    (is= (append '(1) (list 2 '(3))) '(1 2 (3))))
  (testing "null?"
    (isnot (null? (list 1)))
    (is (null? (cdr (list 1))))
    (isnot (null? (cdr (list 1 2))))
    (is (null? (cdr (cdr (list 1 2))))))
  (testing "let"
    (is= (let ((a 1)) a) 1)
    (is= (let ((a (+ 1 1)) (b 3)) (+ a b)) 5))
  (testing "begin"
    (letfn [(container []
                       (clojure.core/let [col (java.util.Vector.)]
                         (fn [v]
                           (.add col v)
                           (.toString col))))]
      (do
        (define (foreach f l) (if (not (null? l)) (begin (f (car l)) (foreach f (cdr l)))))
        (def vs (container))
        (foreach vs [1 2 3])
        (is= (vs 0) "[1, 2, 3, 0]"))))
  (testing "lambda"
    (do
      (define (adder val) (lambda (x) (+ x val)))
      (define add4 (adder 4))
      (is= (add4 4) 8)))
  (testing "display"
    (is= (display (+ 1 2)) nil)))
