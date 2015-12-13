(ns clojure-pl.logic.stlc
  (:refer-clojure :exclude [== reify inc])
  (:use [clojure.core.logic]))

;; stlc expression syntax
;; apply(A,B): The application.
;; [X]>>A: The abstraction.
;; X: A variable.

;; Type expressions have the following syntax:
;; A>B: Function domain

(defna findo [x l o]
       ([_ [[?y :- o] . _] _]
         (project [x ?y] (== (= x ?y) true)))
       ([_ [_ . ?c] _] (findo x ?c o)))

(defn typedo [c x t]
  (conda
    ((lvaro x) (findo x c t))
    ((matche [c x t]
             ([_ [[?x] :>> ?a] [?s :> ?t]]
               (fresh [l]
                      (conso [?x :- ?s] c l)
                      (typedo l ?a ?t)))
             ([_ [:apply ?a ?b] _]
               (fresh [s o]
                      (typedo c ?a [s :> t])
                      (typedo c ?b s)))))))
