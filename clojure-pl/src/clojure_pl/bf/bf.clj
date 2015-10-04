(ns clojure-pl.bf.bf
  (:require [clojure-pl.cota :refer :all])
  (:refer-clojure :exclude [pop!]))

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
             (recur next-cells next-ptr (inc instruct-pointer)))
        \+ (recur (update-in cells [current-cell-pointer] inc)
                  current-cell-pointer
                  (inc instruct-pointer))
        \- (recur (update-in cells [current-cell-pointer] dec)
                  current-cell-pointer
                  (inc instruct-pointer))
        \. (do
             ;;nothing will out put if use print instead
             (.print System/out (char (nth cells current-cell-pointer)))
             (recur cells current-cell-pointer (inc instruct-pointer)))
        \, (let [ch (.read System/in)]
             (recur (assoc cells current-cell-pointer ch)
                    current-cell-pointer
                    (inc instruct-pointer)))
        \[ (recur cells current-cell-pointer (inc (if (zero? (nth cells current-cell-pointer))
                                                    (match-bracket \[ \] instruct-pointer inc)
                                                    instruct-pointer)))
        \] (recur cells current-cell-pointer (inc (if-not (zero? (nth cells current-cell-pointer))
                                                    (match-bracket \] \[ instruct-pointer dec)
                                                    instruct-pointer)))
        nil cells
        (recur cells current-cell-pointer (inc instruct-pointer))))))

(def bf-instructions #{\[ \] \< \> \+ \- \, \.})

(defn pop!
  [stack]
  (let [top-elem (car @stack)]
    (swap! stack pop)
    top-elem))

(defn push!
  [stack item]
  (swap! stack conj item))

(defn parse
  [codes]
  (let [parsed (atom [])
        bracket-map (atom {})
        leftstack (atom '())
        pc (atom 0)]
    (loop [instruct-pointer 0]
      (if (= instruct-pointer (count codes))
        [(clojure.string/join @parsed) @bracket-map]
        (let [ch (get codes instruct-pointer)]
          (when (bf-instructions ch)
            (push! parsed ch)
            (condp = ch
              \[ (push! leftstack @pc)
              \] (let [left (pop! leftstack)
                       right @pc]
                   (swap! bracket-map assoc left right)
                   (swap! bracket-map assoc right left))
              "no match character")
            (swap! pc inc))
          (recur (inc instruct-pointer)))))))

(defn brainfuck-interpreter
  [codes]
  (let [[parsed-program bracket-map] (parse codes)]
    (loop [cells [0N]
           current-cell-pointer 0
           instruct-pointer 0]
      (condp = (get parsed-program instruct-pointer)
        \< (recur cells (dec current-cell-pointer) (inc instruct-pointer))
        \> (let [next-ptr (inc current-cell-pointer)
                 next-cells (if (= next-ptr (count cells))
                              (conj cells 0N)
                              cells)]
             (recur next-cells next-ptr (inc instruct-pointer)))
        \+ (recur (update-in cells [current-cell-pointer] inc)
                  current-cell-pointer
                  (inc instruct-pointer))
        \- (recur (update-in cells [current-cell-pointer] dec)
                  current-cell-pointer
                  (inc instruct-pointer))
        \. (do
             ;;nothing will out put if use print instead
             (.print System/out (char (nth cells current-cell-pointer)))
             (recur cells current-cell-pointer (inc instruct-pointer)))
        \, (let [ch (.read System/in)]
             (recur (assoc cells current-cell-pointer ch)
                    current-cell-pointer
                    (inc instruct-pointer)))
        \[ (recur cells current-cell-pointer (inc (if (zero? (nth cells current-cell-pointer))
                                                    (bracket-map instruct-pointer)
                                                    instruct-pointer)))
        \] (recur cells current-cell-pointer (inc (if-not (zero? (nth cells current-cell-pointer))
                                                    (bracket-map instruct-pointer)
                                                    instruct-pointer)))
        nil cells
        (recur cells current-cell-pointer (inc instruct-pointer))))))

(defn run
  "clojure versionn bf interpreter is slow"
  [filepath]
  ;;(bf-interpreter (slurp filepath))
  (brainfuck-interpreter (slurp filepath)))
