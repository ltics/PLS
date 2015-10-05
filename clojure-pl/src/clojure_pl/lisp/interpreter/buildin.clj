(ns clojure-pl.lisp.interpreter.buildin
  (:require [clojure-pl.cota :refer :all]))

(def eval* (resolve 'clojure-pl.lisp.interpreter.interp/eval*))
(def get-evaled (resolve 'clojure-pl.lisp.interpreter.interp/get-evaled))

;;生成不定参数函数
(defn gen-uncertain-param-fn
  [f name]
  (fn [args env]
    (dprn name " -> " args)
    (let [args (map #(car (eval* % env)) args)]
      [(reduce f args) env])))


