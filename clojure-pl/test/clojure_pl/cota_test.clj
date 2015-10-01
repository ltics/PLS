(ns clojure-pl.cota-test
  (:require [clojure.test :refer :all]
            [clojure-pl.cota :refer :all]))

(deftest cota-test
  (testing "assq can search value from a list of pairs"
    (is= (assq 'a '([a 1] [b 2])) 1)
    (is= (assq 'c (list ['a 1] ['b 2])) nil)))
