(ns clojure-pl.lisp.metainterp.repl
  (:require [clojure-pl.cota :refer :all]
            [clojure-pl.lisp.metainterp.meta :refer :all])
  (:refer-clojure :exclude [cond cons let]))

(defn repl*
  [res]
  (println res)
  (print "=> ")
  (flush)
  (clojure.core/let [l (read-line)]
    (recur (eval (read-string l)))))

(defn repl []
  (repl* "meta interpreter"))

(repl)
