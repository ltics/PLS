(ns clojure-pl.lisp-test
  (:require [acolfut.sweet :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.lisp.interpreter.parser :refer :all]
            [clojure-pl.lisp.interpreter.buildin :refer :all]
            [clojure-pl.lisp.interpreter.interp :refer :all]))

(reset! *debug* false)

(deftest interpreter-test
  (testing "tokenizer test"
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(cleantha)")
         [[:open] [:symbol "cleantha"] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(\"cleantha\")")
         [[:open] [:string "cleantha"] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "+12")
         [[:number "+12"]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "-12")
         [[:number "-12"]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "12")
         [[:symbol "12"]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(+ 1 1)")
         [[:open] [:symbol \+] [:symbol \1] [:symbol \1] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(+ (+ 1 2) 1)")
         [[:open] [:symbol \+] [:open] [:symbol \+] [:symbol \1] [:symbol \2] [:close] [:symbol \1] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "symbol")
         [[:symbol "symbol"]]))
  (testing "parse test"
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "(+ 1 a)") [:+ 1.0 :a])
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "+12") 12.0)
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "\"cleantha\"") "cleantha")
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "\"clean\\\"tha\"") "clean\"tha")
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "kalle olle") :kalle)
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "(kalle +12 24) (olle -12)") [:kalle 12.0 24.0])
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "(not true)") [:not :true]))
  (testing "eval test"
    (testing "primitive"
      (is= (eval* (parse "(+ 1 2)") buildin-env) [3.0 buildin-env])
      (is= (eval* (parse "(> 1 2)") buildin-env) [false buildin-env])
      (is= (eval* (parse "(+ 1 2 3)") buildin-env) [6.0 buildin-env]))
    (testing "not"
      (is= (eval* (parse "(not true)") buildin-env) [(not true) buildin-env]))
    (testing "if"
      (is= (eval* (parse "(if true 1 2)") buildin-env) [1.0 buildin-env])
      (is= (eval* (parse "(if false 1 2)") buildin-env) [2.0 buildin-env]))
    (testing "cond"
      (is= (eval* (parse "(cond (true 1))") buildin-env) [1.0 buildin-env])
      (is= (eval* (parse "(cond (false 1) (true 2))") buildin-env) [2.0 buildin-env])
      (is= (eval* (parse "(cond (false 1) (false 2) (else 3))") buildin-env) [3.0 buildin-env]))
    (testing "cons"
      (is= (eval* (parse "(cons 1)") buildin-env) ['(1.0) buildin-env])
      (is= (eval* (parse "(cons 1 2)") buildin-env) ['(1.0 2.0) buildin-env])
      (is= (eval* (parse "(cons 1 (1 2))") buildin-env) ['(1.0 1.0 2.0) buildin-env])
      (is= (eval* (parse "(cons 1 (cons 1 2))") buildin-env) ['(1.0 1.0 2.0) buildin-env]))
    (testing "list"
      (is= (eval* (parse "(list 1 2)") buildin-env) ['(1.0 2.0) buildin-env])
      (is= (eval* (parse "(list (+ 1 2) (if (> 1 2) 1 2))") buildin-env) ['(3.0 2.0) buildin-env]))
    (testing "append"
      (is= (eval* (parse "(append (1 2) (3 4))") buildin-env) ['(1.0 2.0 3.0 4.0) buildin-env])
      (is= (eval* (parse "(append ((+ 1 2) 2) (3 4))") buildin-env) ['(3.0 2.0 3.0 4.0) buildin-env]))
    (testing "begin"
      (is= (eval* (parse "(begin (+ 1 2))") buildin-env) [3.0 buildin-env])
      (is= (eval* (parse "(begin (+ 1 2) (+ 3 4))") buildin-env) [7.0 buildin-env]))
    (testing "car"
      (is= (eval* (parse "(car ())") buildin-env) [nil buildin-env])
      (is= (eval* (parse "(car (1))") buildin-env) [1.0 buildin-env])
      (is= (eval* (parse "(car (1 2 3))") buildin-env) [1.0 buildin-env]))
    (testing "cdr"
      (is= (eval* (parse "(cdr ())") buildin-env) ['() buildin-env])
      (is= (eval* (parse "(cdr (1))") buildin-env) ['() buildin-env])
      (is= (eval* (parse "(cdr (1 2))") buildin-env) ['(2.0) buildin-env])
      (is= (eval* (parse "(cdr (1 2 3))") buildin-env) ['(2.0 3.0) buildin-env]))
    (testing "null?"
      (is= (eval* (parse "(null? nil)") buildin-env) [true buildin-env])
      (is= (eval* (parse "(null? 1)") buildin-env) [false buildin-env])
      (is= (eval* (parse "(null? (if true 1))") buildin-env) [false buildin-env])
      (is= (eval* (parse "(null? (if false 1))") buildin-env) [true buildin-env]))
    (testing "let"
      (is= (eval* (parse "(let ((a 1) (b 2)) (+ a b))") buildin-env) [3.0 buildin-env]))
    (testing "display & newline"
      (is= (eval* (parse "(display (+ 1 2))") buildin-env) [nil buildin-env])
      (is= (eval* (parse "(newline)") buildin-env) [nil buildin-env]))
    (testing "define"
      (is= (eval* (parse "(add 1 2)") (second (eval* (parse "(define (add a b) (+ a b))") buildin-env)))
           [3.0 (cons {:a 1.0 :b 2.0} (assoc-in buildin-env [0 :add] '((:a :b) [:+ :a :b])))])
      (is= (eval* (parse "(+ a 2)") (second (eval* (parse "(define a 1)") buildin-env)))
           [3.0 (assoc-in buildin-env [0 :a] 1.0)]))
    (testing "lambda"
      (is= (eval* (parse "((lambda (a) (+ a 1)) 1)") buildin-env)
           [2.0 (cons {:a 1.0} buildin-env)])
      (is= (eval* (parse "((lambda (a) (+ a a)) 1)") buildin-env)
           [2.0 (cons {:a 1.0} buildin-env)])))
  (testing "lisp interpreter"
    (letfn [(test-eq-with-env [s r e]
                  (is (= (get-evaled (parse s) e) r)))
            (test-eq [s r]
                  (test-eq-with-env s r buildin-env))
            (get-env-env [s e]
                  (let [[_ env] (eval* (parse s) e)]
                    env))
            (get-env [s]
                  (get-env-env s buildin-env))]
      (testing "number"
        (test-eq "3.14" 3.14))
      (testing "string"
        (test-eq "\"kalle\"" "kalle"))
      (testing "var"
        (test-eq-with-env "kalle" 1 [{:kalle 1}]))
      (testing "add"
        (test-eq "(+ 1 2)" (+ 1.0 2.0))
        (test-eq "(+ 1 (+ 2 3))" (+ 1.0 (+ 2.0 3.0)))
        (test-eq "(+ 1)" (+ 1.0))
        (test-eq "(+ 1 1 1)" (+ 1.0 1.0 1.0))
        (test-eq-with-env "(+ 1 kalle)" 2.0 (conj buildin-env {:kalle 1})))
      (testing "sub"
        (test-eq "(- 1 2)" (- 1.0 2.0))
        (test-eq "(- 1 (- 2 3))" (- 1.0 (- 2.0 3.0)))
        (test-eq "(- 1)" (- 1.0))
        (test-eq "(- 1 1 1)" (- 1.0 1.0 1.0)))
      (testing "mul"
        (test-eq "(* 2 3.14)" (* 2.0 3.14))
        (test-eq "(+ 1 (* 2 3))" (+ 1.0 (* 2.0 3.0)))
        (test-eq "(* 1)" (* 1.0))
        (test-eq "(* 2 1 2 2)" (* 2.0 1.0 2.0 2.0)))
      (testing "div"
        (test-eq "(/ 9 3)" (/ 9.0 3.0))
        (test-eq "(+ 1 (/ 2 3))" (+ 1.0 (/ 2.0 3.0)))
        (test-eq "(/ 1)" (/ 1.0))
        (test-eq "(/ 2)" (/ 2.0))
        (test-eq "(/ 1 2 3)" (/ 1.0 2.0 3.0)))
      (testing "eq"
        (test-eq "(= 2 2)" (= 2 2))
        (test-eq "(= 2 (+ 1 1))" (= 2 (+ 1 1)))
        (test-eq "(= 1)" (= 1))
        (test-eq "(= 1 1 (+ 1 1) 1)" (= 1 1 (+ 1 1) 1)))
      (testing "gt"
        (test-eq "(> 2 2)" (> 2 2))
        (test-eq "(> 1 2)" (> 1 2))
        (test-eq "(> 2 1)" (> 2 1))
        (test-eq "(> (+ 1 1 1) 2)" (> (+ 1 1 1) 2))
        (test-eq "(> 1)" (> 1))
        (test-eq "(> 1 1 (+ 1 1) 1)" (> 1 1 (+ 1 1) 1)))
      (testing "ge"
        (test-eq "(>= 2 2)" (>= 2 2))
        (test-eq "(>= 1 2)" (>= 1 2))
        (test-eq "(>= 2 1)" (>= 2 1))
        (test-eq "(>= (+ 1 1 1) 2)" (>= (+ 1 1 1) 2)))
      (testing "lt"
        (test-eq "(< 2 2)" (< 2 2))
        (test-eq "(< 1 2)" (< 1 2))
        (test-eq "(< 2 1)" (< 2 1))
        (test-eq "(< (+ 1 1 1) 2)" (< (+ 1 1 1) 2)))
      (testing "le"
        (test-eq "(<= 2 2)" (<= 2 2))
        (test-eq "(<= 1 2)" (<= 1 2))
        (test-eq "(<= 2 1)" (<= 2 1))
        (test-eq "(<= (+ 1 1 1) 2)" (<= (+ 1 1 1) 2)))
      (testing "not"
        (test-eq "(not (= 1 1))" false)
        (test-eq "(not (not (= 1 1)))" true))
      (testing "define"
        (test-eq-with-env "lisa" 4.0 (get-env "(define lisa 4)"))
        (test-eq-with-env "nisse" 3.0 (get-env "(define nisse (+ 1 1 1))")))
      (testing "if"
        (test-eq "(if (< 2 1) 10 11)" 11.0)
        (test-eq "(if (< (+ 1 1 1) 1) 11 (* 2 5))" 10.0)
        (test-eq "(if true 1 2)" 1.0)
        (test-eq "(if false 1 2)" 2.0)
        (test-eq "(if false 1)" nil))
      (testing "cond"
        (test-eq "(cond (true 1) ((= 1 2) 2))" 1.0)
        (test-eq "(cond ((= 1 2) 1) (true 2))" 2.0)
        (test-eq "(cond (false 1) (false 2) (else 3))" 3.0)
        (test-eq "(cond (false 1) (false 2))" nil))
      (testing "cons"
        (test-eq "(cons 1 2)" (list 1.0 2.0))
        (test-eq "(cons 1 (cons 2 3))" (list 1.0 2.0 3.0))
        (test-eq "(cons 1 (cons 2 (cons 3 4)))" (list 1.0 2.0 3.0 4.0))
        (test-eq "(cons (cons 1 2) 3)" (list (list 1.0 2.0) 3.0)))
      (testing "list"
        (test-eq "(list 1)" (list 1.0))
        (test-eq "(list 1 2)" (list 1.0 2.0))
        (test-eq "(list 1 (list 2 3) 4)" (list 1.0 (list 2.0 3.0) 4.0))
        (test-eq "(list 1 \"kalle\")" (list 1.0 "kalle"))
        (test-eq "(list)" (list)))
      (testing "append"
        (test-eq "(append (list 1 2))" (list 1.0 2.0))
        (test-eq "(append (list 1 2) (list 3 4))" (list 1.0 2.0 3.0 4.0))
        (test-eq "(append (list 1) (list 2 (list 3)))" (list 1.0 2.0 (list 3.0))))
      (testing "car"
        (test-eq "(car (list 1 2))" (first (list 1.0 2.0)))
        (test-eq "(car (list (list 1) 2))" (first (list (list 1.0) 2.0))))
      (testing "cdr"
        (test-eq "(cdr (list 1))" (rest (list 1.0)))
        (test-eq "(cdr (list 1 2))" (rest (list 1.0 2.0)))
        (test-eq "(cdr (list 1 (list 2 3)))" (rest (list 1.0 (list 2.0 3.0))))
        (test-eq "(cdr (list (list 1)))" (rest (list (list 1.0)))))
      (testing "null?"
        (test-eq "(null? (list 1))" (empty? (list 1.0)))
        (test-eq "(null? (cdr (list 1)))" (empty? (rest (list 1.0))))
        (test-eq "(null? (cdr (cdr (list 1))))" (empty? (rest (rest (list 1.0)))))
        (test-eq "(null? (list))" (empty? (list))))
      (testing "let"
        (test-eq "(let ((a 1)) a)" 1.0)
        (test-eq "(let ((a 1)(b (+ 1 1))) (+ a b))" 3.0))
      (testing "begin"
        (test-eq "(begin 1 2)" 2.0)
        (test-eq "(begin (define x 2) x)" 2.0))
      (testing "function"
        (test-eq-with-env "(add 1 2)" 3.0 (get-env "(define (add a b) (+ a b))"))
        (test-eq-with-env "(fact (+ 5 5))" 3628800.0 (get-env "(define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))"))
        (test-eq-with-env "(add 1 3)" 4.0 (get-env "(define (add a b) (begin (define (worker x y) (+ x y)) (worker a b)))")))
      (testing "lambda"
        (let [e (get-env "(define (adder val) (lambda (x) (+ x val)))")]
          (test-eq-with-env "(add4 4)" 8.0 (get-env-env "(define add4 (adder 4))" e)))
        (test-eq-with-env
          "(map (lambda (x) (* x x)) (list 1 2 3))" (list 1.0 4.0 9.0)
          (get-env "(define (map f l) (if (not (null? l)) (cons (f (car l)) (map f (cdr l)))))"))
        (let [[_ nenv] (eval* (parse "(define (foreach f l) (if (not (null? l)) (begin (f (car l)) (foreach f (cdr l)))))") buildin-env)]
          (eval* (parse "(foreach display (list 1 2 3))") nenv))))))
