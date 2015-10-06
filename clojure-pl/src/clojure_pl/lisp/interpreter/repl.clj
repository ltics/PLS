(ns clojure-pl.lisp.interpreter.repl
  (:require [clojure-pl.lisp.interpreter.parser :refer :all]
            [clojure-pl.lisp.interpreter.buildin :refer :all]
            [clojure-pl.lisp.interpreter.interp :refer :all])
  (:refer-clojure :exclude [list*]))

(defn repl*
  {:private true}
  [[res env]]
  (println res)
  (print "=> ")
  (flush)
  (if-let [line (read-line)]
    (recur (eval* (parse line) env))))

(defn repl []
  (println "lisp interpreter")
  (print "=> ")
  (flush)
  (if-let [line (read-line)]
    (-> line
        parse
        (eval* buildin-env)
        repl*)))