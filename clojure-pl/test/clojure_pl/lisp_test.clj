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
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "kalle olle") :kalle)
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "(kalle +12 24) (olle -12)") [:kalle 12.0 24.0])
    (is= (#'clojure-pl.lisp.interpreter.parser/parse "(not true)") [:not :true]))
  (testing "eval test"
    (is= (eval* (parse "(not true)") buildin-env) [(not true) buildin-env])
    (is= (eval* (parse "(if true 1 2)") buildin-env) [1.0 buildin-env])
    (is= (eval* (parse "(if false 1 2)") buildin-env) [2.0 buildin-env])
    (is= (eval* (parse "(cond (true 1))") buildin-env) [1.0 buildin-env])
    (is= (eval* (parse "(cond (false 1) (true 2))") buildin-env) [2.0 buildin-env])
    (is= (eval* (parse "(cond (false 1) (false 2) (else 3))") buildin-env) [3.0 buildin-env])))
