(ns clojure-pl.logic.trs
  (:refer-clojure :exclude [== reify inc])
  (:use [clojure.core.logic]))

;; a relation is just a function returns a goal result
;; s# is successful goal
;; f# is failed goal

(= (run* [q]
     u#)
   (lazy-seq '()))
;; since u# fails,
;; and because the expression (run∗ (q) g ...) has the value () if any goal in g ... fails.

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

(= (run* [q]
     s#
     (== false q))
   (lazy-seq '(false)))
;; (== false x) will be succeed depend on the value of x

(let [x true]
  (== false x))
;; unify will failed case x is not false

(let [x false]
  (== false x))
;; unify will success case x is false

(= (run* [x]
     (let [x true]
       (== true x)))
   (lazy-seq '(_0)))
;; 因为中间的relation返回的goal是success的
;; 而let又是local bind是一个lambda abstraction
;; 所以外面的fresh变量可以为任何值 就用占位符代替了

(= (run* [x]
     (let [x true]
       (== false x)))
   (lazy-seq '()))
;; 中间的relation的goal是失败 所以外面的fresh变量没有归一值

(= (run* [q]
     (fresh [x]
       (== true x)
       (== true q)))
   (lazy-seq '(true)))
;; because ‘(fresh (x ...) g ...)’ binds fresh variables to x ...
;; and succeeds if the goals g ... succeed.
;; (== v x) succeeds when x is fresh. q, x are all fresh variable

;; the law of fresh => If x is fresh, then (== v x) succeeds and associates x with v.
;; the law of == (unification) => (≡v w)isthesameas(≡w v).

(= (run* [q]
     (fresh [x]
       (== x true)
       (== q true)))
   (lazy-seq '(true)))
;; because the order of arguments to == does not matter.

(= (run* [q]
     s#)
   (lazy-seq '(_0)))
;; value associated with x '_0 is a symbol representing a fresh variable.

(= (run* [x]
     (let [x false]
       (fresh [x]
         (== true x))))
   (lazy-seq '(_0)))
;; sincethex in (== true x) is the one introduced by the fresh expression
;; it is neither the x introduced in the run expression
;; nor the x introduced in the lambda expression(let).

(= (run* [r]
     (fresh [x y]
       (== (cons x (cons y '())) r)))
   (lazy-seq '((_0 _1))))
;; For each different fresh variable there is a symbol with an underscore followed by a numeric subscript.
;; This entity is not a variable but rather is a way of showing that the variable was fresh.
;; We say that such a variable has been reified.

(= (run* [r]
     (fresh [x]
       (let [y x]
         (fresh [x]
           (== (cons x (cons y (cons x '()))) r)))))
   (run* [r]
     (fresh [x]
       (let [y x]
         (fresh [x]
           (== (cons y (cons x (cons y '()))) r)))))
   (lazy-seq '((_0 _1 _0))))
;; Within the inner fresh, x and y are different variables,
;; and since they are still fresh, they get different reified names.
;; Reifying r’s value reifies the fresh variables in the order in which they appear in the list.