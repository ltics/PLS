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
;; if a fresh variable is associated it's no longer fresh and can not be associated any more.
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

(= (run* [q]
     (== false q)
     (== true q))
   (lazy-seq '()))
;; The first goal (== false q) succeeds, associating false with q;
;; true cannot then be associated with q, since q is no longer fresh.

(= (run* [q]
     (== false q)
     (== false q))
   (lazy-seq '(false)))
;; In order for the run to succeed, both (== false q) and (== false q) must succeed
;; The first goal succeeds while associating false with the fresh variable q.
;; The second goal succeeds because although q is no longer fresh, false is already associated with it.

(= (run* [q]
     (let [x q]
       (== true x)))
   (lazy-seq '(true)))
;; because q and x are the same.

(= (run* [q]
     (fresh [x]
       (== x q)))
   (lazy-seq '(_0)))
;; because q starts out fresh and then q gets whatever association that x gets,
;; but both x and q remain fresh.
;; When one fresh variable is associated with another,
;; we say they co-refer or share.

(= (run* [q]
     (fresh [x]
       (== true x)
       (== x q)))
   (lazy-seq '(true)))
;; co-refer law

(= (run* [q]
     (fresh [x]
       (== x q)
       (== true x)))
   (lazy-seq '(true)))
;; because the first goal ensures that whatever association x gets, q also gets.
;; co-refer law

(= (run* [q]
     (fresh [x]
       (== (= x q) q)))
   (lazy-seq '(false)))

(= (run* [q]
     (let [x q]
       (fresh [q]
         (== (= x q) x))))
   (lazy-seq '(false)))
;; Every variable introduced by fresh (or run) is different from every other variable introduced by fresh (or run).

(= (run* [q]
     (conde
       (u# s#)
       (s# u#)))
   (lazy-seq '()))

(= (run* [q]
     (conde
       (s# s#)
       (s# u#)))
   (lazy-seq '(_0)))

(= (run* [q]
     (conde
       (u# s#)
       (s# s#)))
   (lazy-seq '(_0)))

(= (run* [q]
     (conde
       (s# s#)
       (s# s#)))
   (lazy-seq '(_0 _0)))
;; conde is actually the miniKanren's condi. Core.logic offers no conde.
;; This means the order of results may not match what is shown when you use conde with miniKanren.
;; conde does not support defining an else clause. Just use a (s# ...) at the end of your conde.

(= (run* [x]
     (conde
       ((== 'olive x) s#)
       ((== 'oil x) s#)
       (s# u#)))
   (lazy-seq '(olive oil)))

(= (run* [x]
     (conde
       ((== 'olive x) u#)
       ((== 'oil x) s#)
       (s# u#)))
   (lazy-seq '(oil)))

(= (run* [x]
     (conde
       ((== 'olive x) s#)
       ((== 'oil x) s#)
       (s# s#)))
   (lazy-seq '(olive oil _0)))
;; because (== 'olive x) succeeds; therefore, the answer is s#.
;; The s# preserves the association of x to 'olive.
;; To get the second value, we pretend that (== 'olive x) fails;
;; this imagined failure refreshes x. Then (== 'oil x) succeeds.
;; The s# preserves the association of x to 'oil.
;; We then pretend that (== 'oil x) fails, which once again refreshes x.
;; Since no more goals succeed, we are done.

;; the law of conde => To get more values from conde,
;; pretend that the successful conde line has failed,
;; refreshing all variables that got an association from that line.

;; the e in conde stands for every line, since every line can succeed.

(= (run 1 [x]
        (conde
          ((== 'olive x) s#)
          ((== 'oil x) s#)
          (s# u#)))
   (lazy-seq '(olive)))
;; run 1 produces at most one value.

(= (run* [x]
     (conde
       ((== 'virgin x) u#)
       ((== 'olive x) s#)
       (s# s#)
       ((== 'oil x) s#)
       (s# u#)))
   (run* [x]
     (conde
       ((== 'olive x) s#)
       ((s# s#))
       ((== 'oil x) s#)
       (s# u#)))
   (lazy-seq '(olive _0 oil)))
;; once the first conde line fails, it is as if that line were not there.

(= (run 2 [x]
        (conde
          ((== 'extra x) s#)
          ((== 'virgin x) u#)
          ((== 'olive x) s#)
          ((== 'oil x) s#)
          (s# u#)))
   (lazy-seq '(extra olive)))
;; just get first two values

(= (run* [r]
     (fresh [x y]
       (== 'split x)
       (== 'pea y)
       (== (cons x (cons y '())) r)))
   (lazy-seq '((split pea))))

(run* [r]
  (fresh [x y]
    (conde
      ((== 'split x) (== 'pea y))
      ((== 'navy x) (== 'bean y))
      (s# u#))
    (== (cons x (cons y '())) r)))

(run* [r]
  (fresh [x y]
    (conde
      ((== 'split x) (== 'pea y))
      ((== 'navy x) (== 'bean y))
      (s# u#))
    (== (cons x (cons y (cons 'soup '()))) r)))

(defn teacupo
  [x]
  (conde
    ((== 'tea x) s#)
    ((== 'cup x) s#)
    (s# u#)))

(= (run* [x]
     (teacupo x))
   (lazy-seq '(tea cup)))

(= (run* [r]
     (fresh [x y]
       (conde
         ((teacupo x) (== true y) s#)
         ((== false x) (== true y))
         (s# u#))
       (== (cons x (cons y ())) r)))
   (lazy-seq '((false true) (tea true) (cup true))))
;; From (teacupo x), x gets two associations, and from (== false x), x gets one association.

(= (run* [r]
     (fresh [x y z]
       (conde
         ((== y x) (fresh [x] (== z x)))
         ((fresh [x] (== y x)) (== z x))
         (s# u#))
       (== (cons y (cons z ())) r)))
   (lazy-seq '((_0 _1) (_0 _1))))
;; it looks like both occurrences of _0 have come from the same variable
;; and similarly for both occurrences of _1