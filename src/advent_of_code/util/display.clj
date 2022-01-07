(ns advent-of-code.util.display
  (:require [advent-of-code.util.cond-let :refer [cond-let]]
            [clojure.string :as s]))

(defn padding-str [n padchar]
  (->> padchar (repeat n) (apply str)))

(defn adjust [margin len width]
  (cond
    (<= width margin)
    0
    (<= width len)
    0
    (< (- width len) margin)
    (- width len)
    :else
    margin))

(defn pad 
"Pads a string to desired width, justifiying it by 
said margin. Justified can be one of 
:left, :center, :right
"
[^:String s ^:String padding width margin justification tab]
  (let [len (.length s)]
    (if (<= width len) 
      s
      (let [padchar (first (.substring padding 0 1))
            margin (adjust margin len width)
            opposite (- width len margin)
            [left right]
            (condp = justification
              :left
              [(padding-str margin padchar),
               (padding-str opposite padchar)]
              :right
              [(padding-str opposite padchar),
               (padding-str margin padchar)]
              :center
              (let [l (quot (- width len) 2)
                    r (- width len l)]
                [(padding-str l padchar),
                 (padding-str r padchar)]))]
        (str left s right)))))


(defn line [xs & {:keys [sep padding width margin just tab]}]
  (let [xs-s (map str xs)
        sep (or sep " ")
        padding (or padding " ")
        width (or width 
                  (->> xs-s 
                       (sort-by (comp - count)) 
                       first count))
        margin (or margin 0)
        just (or just :left)
        tab (or tab 0)]
    (->> xs-s
         (map #(pad % padding width margin just tab))
         (s/join sep)
         (apply str)
         (#(str (apply str (repeat tab " ")) %)))))
