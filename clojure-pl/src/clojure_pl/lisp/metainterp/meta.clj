(ns clojure-pl.lisp.metainterp.meta
  (:require [clojure-pl.cota :refer :all])
  (:refer-clojure :exclude [cond cons let]))

(defmacro cond
  [& body]
  (when body
    (clojure.core/let
      [[pred# conseq#] (first body)
       rst# (next body)
       pred# (if (= pred# 'else)
               :else pred#)]
      `(if ~pred#
         ~conseq#
         (cond ~@rst#)))))

#_(clojure.walk/macroexpand-all
  '(cond ((< x 0) (- 0 x)) ((= x 0) 100) (else x)))
;;(if (< x 0) (- 0 x) (if (= x 0) 100 (if :else x nil)))

(defmacro define
  [name-with-args body]
  (if (coll? name-with-args)
    `(defn ~(car name-with-args) [~@(rest name-with-args)] ~body)
    `(def ~name-with-args ~body)))

(defmacro let
  [bindings & body]
  (clojure.core/let [bindings# (reduce concat [] bindings)]
    `(clojure.core/let [~@bindings#] ~@body)))

(defmacro begin
  [& body]
  `(do ~@body))

(defmacro lambda
  [args body & _]
  `(fn [~@args] ~body))

(defn cons
  [fst snd]
  (clojure.core/cons
    fst
    (if (coll? snd)
      snd
      [snd])))

(def append concat)
(def null? nil-or-empty?)
(def display println)