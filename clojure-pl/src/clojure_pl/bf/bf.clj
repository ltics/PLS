(ns clojure-pl.bf.bf
  (:require [clojure-pl.cota :refer :all]))

;;instruction-pointer 类似指向图灵机上指令的探头
;;current-cell-pointer 类似指向图灵机当前可访问内存的指针

(defn bf-interpreter
  [codes]
  ;;should find matched bracket from left -> right and right -> left
  (let [match-bracket (fn [open-bracket close-bracket instruction-pointer direction]
                        (loop [i (direction instruction-pointer)
                               opened 0]
                          (condp = (nth codes i)
                            open-bracket (recur (direction i) (inc opened))
                            close-bracket (if (zero? opened)
                                            i
                                            (recur (direction i) (dec opened)))
                            (recur (direction i) opened))))]
    (loop [cells [0N]
           current-cell-pointer 0
           instruct-pointer 0]
      (condp = (get codes instruct-pointer)
        \< (recur cells (dec current-cell-pointer) (inc instruct-pointer))
        \> (let [next-ptr (inc current-cell-pointer)
                 next-cells (if (= next-ptr (count cells))
                             (conj cells 0N)
                             cells)]
             (recur next-cells next-ptr (inc instruct-pointer)))))))
