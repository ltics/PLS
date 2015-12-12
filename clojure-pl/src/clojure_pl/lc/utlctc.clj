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
