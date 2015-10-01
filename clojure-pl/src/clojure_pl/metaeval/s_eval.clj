(ns clojure-pl.metaeval.s-eval
  (:require [clojure-pl.cota :refer :all]))

(defn s-eval
  [env exp]
  (cond
    (nil? exp) exp
    (number? exp) exp
    (string? exp) exp
    (boolean? exp) exp
    (fn? exp) exp
    (symbol? exp) (env exp)
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
                           (map #(s-eval env %) args))))
    :else exp))

(defn m-eval
  [env exp]
  (letfn [(eval-list [env [op & args :as exp]]
                     (condp = op
                       'let (eval-let env exp)
                       'if (let [[predicate consequent alternative] args]
                             (if (m-eval env predicate)
                               (m-eval env consequent)
                               (m-eval env alternative)))
                       'do (when-not (empty? args)
                             (loop [[f & rst] args]
                               (if (empty? rst)
                                 (m-eval env f)
                                 (do
                                   (m-eval env f)
                                   (recur rst)))))
                       (let [op (m-eval env op)
                             eval-args (map #(m-eval env %) args)]
                         (if (fn? op)
                           (apply op eval-args)
                           (cons op (vec eval-args))))))
          (eval-let [env [_ bindings & body]]
                    (->> (apply list `(do ~@body))
                         (m-eval (letbind->env bindings env))))
          ;;this can let mutiple local binds
          (letbind->env [bindings env]
                        (->> (partition 2 bindings)
                             (reduce (fn [env [k v]]
                                       (assoc env k (m-eval env v)))
                                     env)))]
    (cond
      (nil? exp) exp
      (number? exp) exp
      (string? exp) exp
      (boolean? exp) exp
      (fn? exp) exp
      (symbol? exp) (get env exp exp)
      (seq? exp) (eval-list env exp)
      (map? exp) (into (empty exp) (map (fn [[k v]]
                                          [(m-eval env k)
                                           (m-eval env v)]) exp))
      (coll? exp) (into (empty exp) (map #(m-eval env %) exp))
      :else exp)))