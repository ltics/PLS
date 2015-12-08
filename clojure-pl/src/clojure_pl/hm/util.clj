(ns clojure-pl.hm.util)

(defn vmap
  [f mp]
  (->> mp
       (map (fn [[k v]]
              [k (f v)]))
       (into {})))
