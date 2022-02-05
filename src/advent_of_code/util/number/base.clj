(ns advent-of-code.util.number.base
  (:require [clojure.string :refer [trim split]]))

(defonce ^:private int0 (int \0))

(defn- bits->ints [bits]
  (->> bits (mapv #(- (int %) int0))))

(def hex->bin-raw
"0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111")

(def ^:private hex->bin-map
  (->> hex->bin-raw trim (#(split % #"\n"))
       (map #(split % #" = "))
       (map (fn [[hex bin]]
              [(first hex) (->> bin bits->ints)]))
       (into {})))

(def pow2 (iterate #(* 2 %) 1))

(def pow16 (iterate #(* 16 %) 1))


(defn hex->bits
  "Converts a single hex char string into a 4-bit string"
 [hex]
  (->> hex (mapcat hex->bin-map) (apply str)))

(defn binv->dec [binv]
  (->> binv reverse
       (map * pow2)
       (apply +)))

(defn bits->dec 
  "Converts a bit string to a decimal int"
[bits]
  (->> bits bits->ints binv->dec))

(def ^:private hex->dec-map
  (->> hex->bin-map
       (map (fn [[h ints]]
              [h (binv->dec ints)]))
       (into {})))

(defn hex->dec
  "Converts a hex string to a decimal int."
 [hex]
  (->> hex reverse
       (transduce
        (comp
         (map hex->dec-map)
         (map-indexed vector)
         (map (fn [[i v]]
                (->> i (nth pow16)
                     (* v)))))
        +)))

