(ns clojure-pl.lc.utlc
  (:require [clojure-pl.cota :refer :all]))

(def env0 '())

(defn ext-env
  [x v env]
  (cons [x v] env))

(defn lookup
  [x env]
  (let [p (assq x env)]
    (or p x)))

(defrecord Closure [f env])

(defn parse-lambda-exp
  [exp]
  (let [param (car (cadr exp))
        body (last exp)]
    [param body]))

(defn interp*
  [exp env]
  (cond
    (symbol? exp) (lookup exp env)
    (number? exp) exp
    (= (car exp) 'lambda) (->Closure exp env)
    (= (count exp) 2) (let [v1 (interp* (car exp) env)
                            v2 (interp* (cadr exp) env)]
                        (let [[param body] (parse-lambda-exp (:f v1))
                              env1 (:env v1)]
                          (interp* body (ext-env param v2 env1))))
    (= (count exp) 3) (let [op (car exp)
                            v1 (interp* (cadr exp) env)
                            v2 (interp* (caddr exp) env)]
                        (condp = op
                          '+ (+ v1 v2)
                          '- (- v1 v2)
                          '* (* v1 v2)
                          '/ (/ v1 v2)))))

(defn interp
  [exp]
  (interp* exp env0))
