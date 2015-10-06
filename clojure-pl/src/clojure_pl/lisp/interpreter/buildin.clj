(ns clojure-pl.lisp.interpreter.buildin
  (:require [clojure-pl.cota :refer :all]
            [clojure-pl.lisp.interpreter.interp :refer :all]))

;;生成不定参数函数
(defn gen-uncertain-param-fn
  {:private true}
  [f name]
  (fn [args env]
    (dprn name " -> " args)
    (let [args (map #(car (eval* % env)) args)]
      [(reduce f args) env])))

(defn- not*
  [[fst & rst] env]
  (dprn "not " fst " " rst)
  [(not (get-evaled fst env)) env])

(defn- if*
  [[predicate consequent alternative] env]
  (let [pv (get-evaled predicate env)]
    (if pv
      (eval* consequent env)
      (if-not (nil? alternative)
        (eval* alternative env)
        [nil env]))))

(defn- cond*
  [exps env]
  (letfn [(do-exp [[predicate consequent]]
                  (dprn "cond-do-exp" predicate consequent)
                  (if (= predicate :else)
                    [true, (eval* consequent env)]
                    (let [[predicate _] (eval* predicate env)]
                      (if predicate
                        [true (eval* consequent env)]
                        [false nil]))))
          (run-exps [[fst & rst]]
                    (dprn "cond-run-exps" fst rst)
                    (if-not (nil? fst)
                      (let [[status res] (do-exp fst)]
                        (if status
                          res
                          (recur rst)))
                      [nil env]))]
    (dprn "cond -> " exps)
    (run-exps exps)))

(def buildin-env
  [{:not not*
    :if if*
    :cond cond*
    :true true
    :false false}])
