(ns
  clojure-pl.lc.utlctc
  (:require [clojure.core.typed :as t]))

;;'{:a 1} == {:a 1} ':a == :a
;;but wrap a exp in quote can hide compile warning such as can not resolve symbol
(t/defalias AST
  (t/Rec [AST]
    (t/U '{:op    ':lambda
           :param t/Sym
           :body  AST}
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
  `(assert ~predicate (str "syntax error -> " ~syntax)))

(defn bad-input
  [syntax]
  (throw (Exception. (str "syntax error -> " (pr-str syntax)))))

(t/ann analyze [LCSyntax -> AST])
(defn analyze
  [syntax]
  (cond
    (number? syntax) {:op  :const
                      :val syntax}
    (keyword? syntax) {:op  :const
                       :val syntax}
    (symbol? syntax) {:op   :local
                      :name syntax}
    (and (list? syntax)
         (seq? syntax))
    (case (first syntax)
      let (let [_ (check-input syntax (= 3 (count syntax)))
                [_ bind body] syntax
                _ (check-input bind (and (vector? bind)
                                         (= 2 (count bind))))
                [bindname bindvalue] bind
                _ (check-input bindname (symbol? bindname))]
            {:op   :let
             :name bindname
             :init (analyze bindvalue)
             :body (analyze body)})
      lambda (let [_ (check-input syntax (= 3 (count syntax)))
                   [_ params body] syntax
                   _ (check-input params (and (vector? params)
                                              (= 1 (count params))))
                   [param] params
                   _ (check-input param (symbol? param))]
               {:op    :lambda
                :param param
                :body  (analyze body)})
      if (let [_ (check-input syntax (= 4 (count syntax)))
               [_ pred then else] syntax]
           {:op   :if
            :pred (analyze pred)
            :then (analyze then)
            :else (analyze else)})
      (cond
        (= 2 (count syntax)) (let [[rator rand] syntax]
                               {:op    :app
                                :rator (analyze rator)
                                :rand  (analyze rand)})
        :else (bad-input syntax)))
    :else (bad-input syntax)))

