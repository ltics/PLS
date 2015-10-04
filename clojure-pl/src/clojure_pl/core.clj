(ns clojure-pl.core
  (:require [clojure-pl.bf.bf :refer [run] :as bf]
            [clojure-pl.forth.forth :refer [init-struct] :as forth]
            [clojure-pl.lsystem.lsystem :refer [draw-dragon draw-pentigree draw-triangle]])
  (:gen-class))

(defn -main
  [& args]
  (if args
    (condp = (first args)
      "bf" (let [filepath (second args)]
             (bf/run filepath))
      "forth" (do (println "mini forth lang =>")
                  (forth/repl (init-struct)))
      "lsystem" (condp = (second args)
                  "dragon" (draw-dragon)
                  "pentigree" (draw-pentigree)
                  "triangle" (draw-triangle)))))
