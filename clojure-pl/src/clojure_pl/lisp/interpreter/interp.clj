(ns clojure-pl.lisp.interpreter.interp
  (:require [clojure-pl.cota :refer :all]))

(declare eval*)
(declare apply*)

(defn lookup-env
  {:private true}
  [key env]
  (letfn [(lookup-env* [[fst & rst]]
                       (if (contains? fst key)
                         (key fst)
                         (when-not (nil? rst)
                           (recur rst))))]
    (dprn "lookup key ->" key)
    (let [value (lookup-env* env)]
      (if-not (nil? value)
        value
        (throw (Exception. (format "unbound symbol %s" key)))))))

(defmacro get-evaled
  [exp env]
  `(car (eval* ~exp ~env)))

(defn eval*
  [exp env]
  (dprn "eval ->" exp)
  (cond
    (or (number? exp)
        (string? exp)
        (nil? exp)
        (fn? exp))
    [exp env]

    (keyword? exp)
    [(lookup-env exp env) env]

    (vector? exp)
    (let [[fst & rst] exp
          [r e] (eval* fst env)]
      (dprn "comb" fst rst "(" r e ")")
      (cond
        (fn? r) (apply* r rst e)
        ;;when call a defined function will trigger this out
        (list? r) (let [[args body] r
                        ;;n is a local bind env
                        n (zipmap args (map #(get-evaled % e) rst))
                        new-env (cons n e)]
                    (eval* body new-env))
        :else [(cons r rst) env]))

    :else
    (throw (Exception. (format "invalid interpreter state %s %s" (str exp) (str env))))))

(defn apply*
  [f args env]
  (dprn "apply ->" f args)
  (f args env))
