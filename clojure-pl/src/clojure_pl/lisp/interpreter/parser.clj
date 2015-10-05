(ns clojure-pl.lisp.interpreter.parser
  (import (java.lang.Character)))

;;keypoints to understand tokenizer
;;(let [[fst snd & rst] ")"] xxx) fst -> \) snd -> nil rst -> nil
;;(let [[fst & rst] "a"] (prn fst) xxx) fst -> \a rst -> nil
;;(apply str nil) => ""
;;(str nil "") => ""

(defn tokenizer
  {:private true}
  [codes]
  (letfn [(string [acc [fst snd & rst]]
                  (let [rst (apply str rst)]
                    (cond
                      ;;the close bracket of a string
                      (= fst \") [acc (str snd rst)]
                      (not (nil? fst)) (recur (str acc fst) (str snd rst))
                      :else (throw (Exception. (str "can not tokenize ->" codes))))))
          (token [acc [fst & rst :as s]]
                 (let [rst (apply str rst)]
                   (cond
                     ;;这样扔回到外面也会满足第一个条件从而终止tokenizer
                     (nil? fst) [acc ""]
                     ;;如果是右括号还要原样扔到外部去conj上一个[:close]
                     (= fst \)) [acc s]
                     ;;white space is the end of a symbol
                     (Character/isWhitespace fst) [acc rst]
                     :else (recur (str acc fst) rst))))
          (tokenize* [acc [fst snd & rst]]
                       (let [rst (apply str rst)
                             rst2 (str snd rst)]
                         (cond
                           ;;if codes is [] or "" fst will be nil after deconstruct
                           (nil? fst) acc
                           ;;ignore whitespace
                           (Character/isWhitespace fst) (recur acc rst2)
                           (= fst \() (recur (conj acc [:open]) rst2)
                           (= fst \)) (recur (conj acc [:close]) rst2)
                           (= fst \") (let [[s t] (string "" rst2)]
                                        (recur (conj acc [:string s]) t))
                           (and (= fst \-) (Character/isDigit snd))
                           (let [[n t] (token (str \- snd) rst)]
                             (recur (conj acc [:number n]) t))
                           (and (= fst \+) (Character/isDigit snd))
                           (let [[n t] (token (str \+ snd) rst)]
                             (recur (conj acc [:number n]) t))
                           :else (let [[s t] (token fst rst2)]
                                   (recur (conj acc [:symbol s]) t)))))]
    (tokenize* [] codes)))
