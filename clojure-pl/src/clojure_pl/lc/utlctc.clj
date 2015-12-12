(ns
  clojure-pl.lc.utlctc
  (:require [clojure.core.typed :as t]))

(t/defalias AST
  (t/Rec [AST]
    (t/U '{:op   ':lambda
           :name t/Sym
           :body AST}
         '{:op   ':if
           :pred AST
           :then AST
           :else AST}
         '{:op   ':local
           :name t/Sym}
         '{:op   ':let
           :name t/Sym
           :init AST
           :body AST}
         '{:op  ':const
           :val t/Any}
         '{:op    ':app
           :rator AST
           :rand  AST})))

(t/defalias LCSyntax
  "lc expression syntax"
  t/Any)

(t/defalias CLJSyntax
  "clj expression syntax"
  t/Any)

(defmacro check-input
  [syntax predicate]
  `(assert predicate (str "syntax error -> " ~syntax)))

(defn bad-input
  [syntax]
  (throw (Exception. (str "syntax error -> " (pr-str syntax)))))
