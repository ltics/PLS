(ns clojure-pl.core
  (:require [clojure-pl.forth.forth :refer [init-struct] :as forth])
  (:gen-class))

(defn -main
  [& args]
  (if args
    (condp = (first args)
      "forth" (do (println "mini forth lang =>")
                  (forth/repl (init-struct)))
      "lsystem" nil)))
