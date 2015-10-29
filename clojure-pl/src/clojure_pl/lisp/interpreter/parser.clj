(ns clojure-pl.lisp.interpreter.parser
  (:require [clojure-pl.cota :refer :all])
  (import (java.lang.Character)))

;;keypoints to understand tokenizer
;;(let [[fst snd & rst] ")"] xxx) fst -> \) snd -> nil rst -> nil
;;(let [[fst & rst] "a"] (prn fst) xxx) fst -> \a rst -> nil
;;(apply str nil) => ""
;;(str nil "") => ""
;;(str nil nil) => ""

(defn tokenizer
  {:private true}
  [codes]
  (letfn [(string [acc [fst snd & rst]]
                  (let [rst (apply str rst)]
                    (cond
                      (and (= fst \\) (= snd \")) (recur (str acc "\"") rst)
                      ;;the close bracket of a string 开头的那个\"已经在tokenize*函数中被消费掉了 这里判断字符串的闭合\"
                      (= fst \") [acc (str snd rst)]
                      (not (nil? fst)) (recur (str acc fst) (str snd rst))
                      :else (throw (Exception. (str "can not tokenize -> " codes))))))
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
                         ;;extract number
                         (and (or (= fst \-)
                                  (= fst \+))
                              (Character/isDigit snd))
                         (let [[n t] (token (str fst snd) rst)]
                           (recur (conj acc [:number n]) t))

                         :else (let [[s t] (token fst rst2)]
                                 (recur (conj acc [:symbol s]) t)))))]
    (tokenize* [] codes)))

;;keypoints to understand parse*
;;(let [[fst & rst] []] body) fst -> nil rst -> nil
;;(let [[a b] nil] body) a -> nil b -> nil

(defn parse*
  {:private true}
  [codes]
  (letfn [(map-token [[token value]]
                     (condp = token
                       ;;数字统一处理成双精度浮点型
                       :number (Double/parseDouble value)
                       :string value
                       :symbol (try
                                 (Double/parseDouble (str value))
                                 (catch Exception _ (keyword (str value))))
                       (throw (Exception. "syntax error"))))
          (parse** [acc tokens]
                   (let [[fst & rst] tokens
                         [token _] fst]
                     (cond
                       ;;解析结束之后就将解析结果返回
                       (nil? token) acc
                       ;;如果是open bracket那就先解析下一个token
                       (= token :open) (let [[e t] (parse** [] rst)]
                                         (recur (conj acc e) t))
                       ;;如果是close bracket就返回到配对的open bracket处继续解析
                       (= token :close) [acc rst]
                       :else (recur (conj acc (map-token fst)) rst))))]
    (parse** [] (tokenizer codes))))

(defn parse
  [codes]
  (car (parse* codes)))
