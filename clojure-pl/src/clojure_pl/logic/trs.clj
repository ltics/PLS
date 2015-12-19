(ns clojure-pl.logic.trs
  (:refer-clojure :exclude [== reify inc])
  (:use [clojure.core.logic]))

;; a relation is just a function returns a goal result
;; s# is successful goal
;; f# is failed goal

(= (run* [q]
     u#)
   (lazy-seq '()))
;; since u# fails, and because the expression (run∗ (q) g ...) has the value () if any goal in g ... fails.

(= (run* [q]
     (== s# q))
   (lazy-seq `(~s#)))
;; unification is alwayse success and q is associated with s# goal

(= (run* [q]
     u#
     (== true q))
   (lazy-seq '()))
;; because the expression (run∗ (q) g ... (≡ true q)) has the value () if the goals g ... fail.

(= (run* [q]
     s#
     (== true q))
   (lazy-seq '(true)))
;; because the expression (run∗ (q) g ... (≡ true q)) associates true with q if the goals g ... and (≡ true q) succeed.