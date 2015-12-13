(ns clojure-pl.logic-test
  (:refer-clojure :exclude [== reify inc])
  (:require [acolfut.sweet :refer :all]
            [clojure-pl.cota :refer :all]
            [clojure-pl.logic.stlc :refer :all]
            [clojure.core.logic :refer [run* fresh ==] :as logic]))

(deftest logic-test
  (testing "stlc"
    (is= (lazy-seq '([_0 :> _1]))
         (run* [q]
           (fresh [f g a b t]
             (typedo [[f :- a]
                      [g :- b]]
                     [:apply f g]
                     t)
             (== q a))))
    (is= (lazy-seq '([:int :> _0]))
         (run* [q]
           (fresh [f g a t]
             (typedo [[f :- a]
                      [g :- :int]]
                     [:apply f g]
                     t)
             (== q a))))
    (is= (lazy-seq '(:int))
         (run* [q]
           (fresh [f g a t]
             (typedo [[f :- [:int :> :float]]
                      [g :- a]]
                     [:apply f g]
                     t)
             (== q a))))
    (is= (lazy-seq '())
         (run* [t]
           (fresh [f a b]
             (typedo [f :- a]
                     [:apply f f]
                     t))))
    (is= (lazy-seq '([_0 :> [[_0 :> _1] :> _1]]))
         (run* [t]
           (fresh [x y]
             (typedo []
                     [[x] :>> [[y] :>> [:apply y x]]]
                     t))))))
