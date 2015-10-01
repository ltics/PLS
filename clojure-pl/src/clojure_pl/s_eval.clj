(ns clojure-pl.s-eval
  (:require [clojure-pl.cota :refer :all]))

(defn s-eval
  [env exp]
  (cond
    (nil? exp) exp
    (number? exp) exp
    (string? exp) exp
    (symbol? exp) (env exp)
    (boolean? exp) exp
    (fn? exp) exp
    (list? exp) (let [[op & args] exp]
                  (condp = op
                    'if (let [[predicate consequent alternative] args]
                          (if (s-eval env predicate)
                            (s-eval env consequent)
                            (s-eval env alternative)))
                    'do (when-not (empty? args)
                          (loop [[f & rst] args]
                            (if (empty? rst)
                              (s-eval env f)
                              (do
                                (s-eval env f)
                                (recur rst)))))
                    ;;just let with single local bind
                    'let (let [[[var val] & body] args]
                           (s-eval (assoc env var val)
                                   (apply list `(do ~@body))))
                    (apply (s-eval env op)
                           (map #(s-eval env %) args))))))
