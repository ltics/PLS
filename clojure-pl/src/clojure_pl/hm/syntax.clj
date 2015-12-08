(ns clojure-pl.hm.syntax)

(def variable? symbol?)

(defn lambda?
  [x]
  (and (seq? x)
       (= 'fn (first x))))

(defn let?
  [x]
  (and (seq? x)
       (= 'let (first x))))

(defn atom?
  "number boolean thunk lambda"
  [x]
  (or (number? x)
      (instance? java.lang.Boolean x)
      (instance? clojure.lang.IDeref x)
      (fn? x)))

(defn application?
  [x]
  (and (seq? x)
       (not (contains? #{'fn 'let} (first x)))))
