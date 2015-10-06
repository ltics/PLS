(ns clojure-pl.core
  (:require [clojure-pl.cota :refer :all]
            [clojure-pl.bf.bf :refer [run] :as bf]
            [clojure-pl.forth.forth :refer [init-struct] :as forth]
            [clojure-pl.lsystem.lsystem :refer [draw-dragon draw-pentigree draw-triangle]]
            [clojure-pl.lisp.interpreter.repl :as lisp])
  (:gen-class))

(reset! *debug* false)

(defn -main
  [& args]
  (if args
    (condp = (first args)
      "lisp" (lisp/repl)
      "bf" (let [filepath (second args)]
             (bf/run filepath))
      "forth" (do (println "mini forth lang =>")
                  (forth/repl (init-struct)))
      "lsystem" (condp = (second args)
                  "dragon" (draw-dragon)
                  "pentigree" (draw-pentigree)
                  "triangle" (draw-triangle)))))
