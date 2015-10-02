(ns clojure-pl.forth.forth
  (:require [clojure-pl.cota :refer :all])
  (:import (java.util Scanner))
  (:refer-clojure :exclude [pop!]))

(declare forth-eval)

(defn pop!
  [stack]
  (let [top-elem (car @stack)]
    (swap! stack pop)
    top-elem))

(defn push!
  [stack item]
  (swap! stack conj item))

(defn next-token
  [stream]
  (if (. stream hasNextInt)
    (. stream nextInt)
    (. stream next)))

(defn gen-f
  [op stack]
  #(let [a (pop! stack)
         b (pop! stack)]
    (push! stack (op b a))))

(defn init-struct []
  (let [stream (Scanner. System/in)
        stack (atom '())
        env (atom {})
        extend-env (fn [var f] (swap! env assoc var f))]
    (extend-env ".s" #(do (println (apply str (repeat 6 "-")))
                          (doseq [elem @stack] (prn elem))
                          (println (apply str (repeat 6 "-")))))
    ;;just start a new line
    (extend-env "cr" #(prn))
    (extend-env "+" (gen-f + stack))
    (extend-env "-" (gen-f - stack))
    (extend-env "*" (gen-f * stack))
    (extend-env "/" (gen-f / stack))
    ;;just duplicate the top elem
    (extend-env "dup" #(push! stack (car @stack)))
    (extend-env "." #(prn (pop! stack)))
    (extend-env ":" #(let [name (next-token stream)
                           block (loop [b []
                                        n (next-token stream)]
                                   (if (= n ";")
                                     b
                                     (recur (conj b n) (next-token stream))))]
                      (extend-env name (fn [] (doseq [token block]
                                                (forth-eval env stack token))))))
    [env stack stream]))

(defn forth-eval
  [env stack token]
  (cond
    (contains? @env token) ((@env token))
    (number? token) (push! stack token)
    :else (prn "unknown token " token)))

(defn repl
  [struct]
  (let [[env stack stream] struct
        token (next-token stream)]
    (when-not (= token "quit")
      (forth-eval env stack token)
      (repl struct))))
