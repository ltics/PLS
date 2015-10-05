(ns clojure-pl.lisp-test
  (:require [acolfut.sweet :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.lisp.interpreter.parser :refer :all]))

(deftest interpreter-test
  (testing "tokenizer"
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(cleantha)")
         [[:open] [:symbol "cleantha"] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(\"cleantha\")")
         [[:open] [:string "cleantha"] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "+12")
         [[:number "+12"]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(+ 1 1)")
         [[:open] [:symbol \+] [:symbol \1] [:symbol \1] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "(+ (+ 1 2) 1)")
         [[:open] [:symbol \+] [:open] [:symbol \+] [:symbol \1] [:symbol \2] [:close] [:symbol \1] [:close]])
    (is= (#'clojure-pl.lisp.interpreter.parser/tokenizer "symbol")
         [[:symbol "symbol"]])))
