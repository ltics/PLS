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

(def buildin-env
  [{:not not*
    :true true
    :false false}])
