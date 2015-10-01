(ns clojure-pl.meta-eval-test
  (:require [clojure.test :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.metaeval.s-eval :refer :all]))

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

(deftest m-eval-test
  (testing "nil evals to nil"
    (is (= nil (m-eval {} nil))))
  (testing "numbers eval to themselves"
    (is (= 1 (m-eval {} 1)))
    (is (= 2 (m-eval {} 2))))
  (testing "strings eval to themselves"
    (is (= "hello" (m-eval {} "hello"))))
  (testing "symbols eval to their value in the environment"
    (is (= 10 (m-eval {'x 10} 'x)))
    (is (= "yo!" (m-eval {'x "yo!"} 'x)))
    (is (= nil (m-eval {'x nil} 'x))))
  (testing "functions evaluate to themselves"
    (is (= + (m-eval {} +)))
    (is (= * (m-eval {} *))))
  (testing "if statements with nil test eval else"
    (is (= 1 (m-eval {} '(if nil 0 1)))))
  (testing "if statements with non-nil test eval then"
    (is (= 0 (m-eval {'t 1 'x 0 'y 1} '(if t x y)))))
  (testing "empty do statement evals to nil"
    (is (= nil (m-eval {} '(do)))))
  (testing "one-expression do evals to expression"
    (is (= 1 (m-eval {} '(do 1)))))
  (testing "multi-expression do evals to last"
    (is (= 4 (m-eval {} '(do 1 2 3 4)))))
  (testing "let statements add to environment"
    (is (= 1 (m-eval {} '(let [x 1] x)))))
  (testing "let statements have implicit do"
    (is (= 10 (m-eval {} '(let [x 10] "hello" x)))))
  (testing "function application evals the args,
               then applies function to evaled args (use map and apply)"
    (is (= 2 (m-eval {'+ +} '(+ 1 1))))
    (is (= 5 (m-eval {'/ /} '(/ 20 4)))))
  (testing "ensure we're calling m-eval recursively"
    (is (= 3 (m-eval {'+ +} '(do (+ 1 2)))))
    (is (= 13 (m-eval {'+ +} '(+ (+ 1 10) 2)))))
  (testing "do expressions evaluate all forms"
    (is (= 85 (m-eval {'deref deref 'reset! reset! 'a (atom nil) '+ +}
                      '(do (reset! a 10) (+ (deref a) 75))))))
  (testing "nested let without shadowing renders the correct value"
    (is (= '(* 1 (+ 5 3)) (m-eval {'x 1 'y 2}
                                  '(* x (+ (let [z 5] z) 3))))))
  (testing "nested let wit shadowing renders the correct value"
    (is (= '(* 1 (+ 5 3)) (m-eval {'x 1 'y 2}
                                  '(* x (+ (let [y 5] y) 3)))))
    (is (= '(* 1 (- (inc 3) 10) (+ 5 3)
               (m-eval {'x 1 'y 2}
                       '(* x (let [y 3 z 10] (- (inc y) z)) (+ (let [y 5] y) 3)))))))
  (testing "resolved nested funcs with nested let without shadowing give correct value"
    (is (= 16 (m-eval {'x 1 'y 2 '+ + '* *}
                      '(* x y (+ (let [z 5] z) 3)))))
    (is (= 18 (m-eval {'x 1 'y 2 '+ + '* * 'inc inc}
                      '(* x y (+ (let [z 5] (inc z)) 3))))))
  (testing "resolved nested funcs with nested let without shadowing give correct value"
    (is (= 24 (m-eval {'x 1 'y 2 '+ + '* *}
                      '(* x (let [y 3] y) (+ (let [y 5] y) 3)))))
    (is (= 28 (m-eval {'x 1 'y 2 '+ + '* * 'inc inc 'dec dec}
                      '(* x (let [y 3] (inc y))
                          (+ (let [y 5] (dec y)) 3))))))
  (testing "nested ifs and let with yield correct value"
    (is (= 1 (m-eval {'x 1} '(let [y 2 z (if (zero? x) 1 0)] z))))
    (is (= 0 (m-eval {'x 1 'zero? zero?} '(let [y 2 z (if (zero? x) 1 0)] z)))))
  (testing "flat and nested seqs with no bindings should be eval'ed"
    (is (= '(1 2 3) (m-eval {} '(1 2 3))))
    (is (= '(1 [2 3] 4) (m-eval {} '(1 [2 3] 4)))))
  (testing "flat and nested seqs with bindings should be eval'ed"
    (is (= '(1 2 3) (m-eval {'x 1 'y 2} '(x y 3))))
    (is (= '[1 [2 3] 4] (m-eval {'x 1 'y 2} '[x [y 3] 4])))
    (is (= '[1 (2 3) 4] (m-eval {'x 1 'y 2} '[x (y 3) 4])))
    (is (= '[1 (2 3) 4] (m-eval {'x 1} '[x ((let [y 2] y) 3) 4]))))
  (testing "flat and nested maps with no bindings should be eval'ed"
    (is (= {:a 1 :b 2} (m-eval {} '{:a 1 :b 2})))
    (is (= {:a {:b 2} :c #{3}})))
  (testing "flat and nested maps with bindings should be eval'ed"
    (is (= {:a 1 :b 2} (m-eval {'x 1 'y 2} '{:a x :b y})))
    (is (= {:a 1 :b 2 :c 3} (m-eval {'x 1 'y 2 '+ +} '{:a x :b y (let [z :c] z) (+ x y)}))))
  (testing "previous bindings local to current let should be merged in enclosing scope bindings and take priority"
    (is (= 3 (m-eval {} '(let [x 3 y x] y))))
    (is (= 5 (m-eval {'x 1 '+ +} '(let [x 2 y (+ x 1)] (+ y x)))))))
