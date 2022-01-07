(ns advent-of-code.-2021.day16.util
  ;; (:refer-clojure :rename {split-at core-split-at})
  (:require [advent-of-code.-2021.day16.input
             :refer [hex->bin-raw]]
            [clojure.string :refer [split trim]]))


(defonce int0 (int \0))
(defn bits->ints [bits]
  (->> bits (mapv #(- (int %) int0))))

(def hex->bin 
  (->> hex->bin-raw trim (#(split % #"\n"))
       (map #(split % #" = "))
       (map (fn [[hex bin]]
              [(first hex) (->> bin bits->ints)]))
       (into {})))

(def pow2 (iterate #(* 2 %) 1))

(defn hex->bits [hex]
  (->> hex (mapcat hex->bin) (apply str)))

(defn pr-bits [hex]
  (println (hex->bits hex)))

(defn binv->dec [binv]
  (->> binv reverse
       (map * pow2)
       (apply +)))

(defn bits->dec [bits]
  (->> bits bits->ints binv->dec))

(defn split-str-at [n s]
  (->> s (split-at n)
       (mapv #(apply str %))))
