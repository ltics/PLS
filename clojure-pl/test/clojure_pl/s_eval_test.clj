(ns clojure-pl.s-eval-test
  (:require [clojure.test :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.s-eval :refer :all]))

(deftest s-eval-test
  (testing "nil evals to nil"
    (is= (s-eval {} nil) nil))
  (testing "numbers eval to themselves"
    (is= (s-eval {} 1) 1)
    (is= (s-eval {} 2) 2))
  (testing "strings eval to themselves"
    (is= (s-eval {} "hello") "hello"))
  (testing "symbols eval to their value in the environment"
    (is= (s-eval {'x 10} 'x) 10)
    (is= (s-eval {'x "yo!"} 'x) "yo!")
    (is= (s-eval {'x nil} 'x) nil))
  (testing "functions evaluate to themselves"
    (is= (s-eval {} +) +)
    (is= (s-eval {} *) *))
  (testing "if statements with false test eval else"
    (is= (s-eval {} '(if false 0 1)) 1)
    (is= (s-eval {'t 1 'x 0 'y 1} '(if t x y)) 0))
  (testing "empty do statement evals to nil"
    (is= (s-eval {} '(do)) nil))
  (testing "one-expression do evals to expression"
    (is= (s-eval {} '(do 1)) 1))
  (testing "multi-expression do evals to last"
    (is= (s-eval {} '(do 1 2 3 4)) 4))
  (testing "let statements add to environment"
    (is= (s-eval {} '(let [x 1] x)) 1))
  (testing "let statements have implicit do"
    (is= (s-eval {} '(let [x 10] "hello" x)) 10))
  (testing "function application evals the args, then applies function to evaled args (use map and apply)"
    (is= (s-eval {'+ +} '(+ 1 1)) 2)
    (is= (s-eval {'/ /} '(/ 20 4)) 5)
    (is= (s-eval {'+ + 'x 1 'y 2} '(+ x y)) 3))
  (testing "ensure we're calling s-eval recusively"
    (is= (s-eval {'+ +} '(do (+ 1 2))) 3)
    (is= (s-eval {'+ +} '(+ (+ 1 10) 2)) 13))
  (testing "do expressions evaluate all forms"
    (is= (s-eval {'deref deref 'reset! reset! 'a (atom nil) '+ +} '(do (reset! a 10) (+ (deref a) 75))) 85)))
