(ns clojure-pl.mk.mukr
  (:require [clojure.string :as string])
  (:import [clojure.lang Seqable]))

;; we just need a idiomic cons just like scheme in the old way
;; in scheme cons can construct a pair or list
(declare seq-cons-list proper-list?)

(defprotocol ICons
  (car [self])
  (cdr [self]))

(deftype Cons [a d]
  ICons
  (car [_] a)
  (cdr [_] d)

  Object
  (toString [self]
    ;; cause cons here will generate a pair or a list depend on the second param
    (if (proper-list? self)
      (format "(%s)"
              (string/join " " (map #(if (nil? %) "nil" (.toString %)) self)))
      (format "(%s . %s)" a d)))

  Seqable
  (seq [self] (seq-cons-list self)))

(defn proper-list? [cell]
  (or (nil? cell)
      (and (list? cell)
           (proper-list? (cdr cell)))))

(defn seq-cons-list [cell]
  (when-not (nil? cell)
    (clojure.core/cons (car cell) (cdr cell))))
