(ns clojure-pl.lisp.interpreter.buildin
  (:require [clojure-pl.cota :refer :all]
            [clojure-pl.lisp.interpreter.interp :refer :all])
  (:refer-clojure :exclude [list*]))

;;生成不定参数函数
(defn gen-uncertain-param-fn
  {:private true}
  [f name]
  (fn [args env]
    (dprn name "->" args)
    (let [args (map #(car (eval* % env)) args)]
      [(apply f args) env])))

(defn- not*
  [[fst & rst] env]
  (dprn "not" fst rst)
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
    (dprn "cond ->" exps)
    (run-exps exps)))

(defn- cons*
  [[fst snd] env]
  (dprn "cons ->" fst snd)
  (let [fval (get-evaled fst env)
        sval (get-evaled snd env)]
    (if (nil? sval)
      [(list fval) env]
      (if ((some-fn seq? vector? list?) sval)
        [(cons fval sval) env]
        [(list fval sval) env]))))

(defn- list*
  [exps env]
  (letfn [(do-exps [acc [fst & rst]]
                   (dprn "list-do-exps" acc fst rst)
                   (if-not (nil? fst)
                     (recur
                       (conj acc (get-evaled fst env))
                       rst)
                     acc))]
    (dprn "list" exps)
    [(->> (do-exps [] exps)
          (into '())
          reverse) env]))

(defn- append*
  [exps env]
  (letfn [(do-exps [acc [fst & rst]]
                   (dprn "append-do-exps" acc fst rst)
                   (if-not (nil? fst)
                     (recur
                       (concat acc (get-evaled fst env))
                       rst)
                     acc))]
    (dprn "append" exps)
    [(do-exps [] exps) env]))

(defn- begin*
  [exps env]
  (letfn [(do-exps [[fst & rst] env r]
                   (dprn "begin-do-exps" fst rst env r)
                   (if-not (nil? fst)
                     (let [[r e] (eval* fst env)]
                       (recur rst e r))
                     [r env]))]
    (dprn "begin" exps)
    (do-exps exps env nil)))

;;key to understand
;;(let [[fst] [1 2 3]] fst) => 1
(defn- car*
  [[lst] env]
  (dprn "car" lst)
  [(car (get-evaled lst env)) env])

(defn- cdr*
  [[lst] env]
  (dprn "cdr" lst)
  [(cdr (get-evaled lst env)) env])

(defn- null?
  [[v] env]
  (dprn "null?" v)
  (let [r (get-evaled v env)]
    [(nil-or-empty? r) env]))

(defn- let*
  [[binds body] env]
  (letfn [(do-bind [acc [fst & rst]]
                   (dprn "let-do-bind" acc fst rst)
                   (if-not (nil? fst)
                     (let [[name value] fst]
                       (recur
                         (assoc acc name (get-evaled value env))
                         rst))
                     acc))]
    (dprn "let" binds body)
    (let [new-env (cons (do-bind {} binds) env)]
      (dprn "let-body" body new-env)
      [(get-evaled body new-env) env])))

(defn- display*
  [[lst] env]
  (dprn "display" lst)
  (println (get-evaled lst env))
  [nil env])

(defn- newline*
  [[lst] env]
  (dprn "newline" lst)
  (println)
  [nil env])

(defn- define*
  [[name body] env]
  (dprn "define" name body)
  (if (vector? name)
    ;;define a function even it is recursive
    (let [[func-name & args] name
          new-env (assoc (first env) func-name (list args body))]
      [nil (cons new-env (rest env))])
    ;;define a variable
    (let [new-env (assoc (first env) name (get-evaled body env))]
      [nil (cons new-env (rest env))])))

(defn- lambda*
  [[args body] env]
  (letfn [(do-body [acc args [fst & rst]]
                   (dprn "lambda-do-body" acc args fst)
                   (if-not (nil? fst)
                     (if-not (nil? (args fst))
                       (recur (conj acc fst) args rst)
                       (let [fval (get-evaled fst env)]
                         (if-not (nil? fval)
                           (recur (conj acc fval) args rst)
                           (throw (Exception. (format "unbound symbol %s" fst))))))
                     acc))]
    (dprn "lambda" args body env)
    [(list (seq args) (do-body [] (set args) body)) env]))

(def buildin-env
  [{:+ (gen-uncertain-param-fn + "add")
    :- (gen-uncertain-param-fn - "sub")
    :* (gen-uncertain-param-fn * "mul")
    :/ (gen-uncertain-param-fn / "div")
    := (gen-uncertain-param-fn = "eq")
    :> (gen-uncertain-param-fn > "gt")
    :>= (gen-uncertain-param-fn >= "gte")
    :< (gen-uncertain-param-fn < "lt")
    :<= (gen-uncertain-param-fn <= "lte")
    :not not*
    :if if*
    :cond cond*
    :cons cons*
    :list list*
    :append append*
    :begin begin*
    :car car*
    :cdr cdr*
    :null? null?
    :let let*
    :display display*
    :newline newline*
    :define define*
    :lambda lambda*
    :nil ""
    :true true
    :false false}])
