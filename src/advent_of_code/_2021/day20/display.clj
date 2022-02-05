(ns advent-of-code.-2021.day20.display
  (:require [clojure.string :refer [join]]))

(defn ->str [img fill vmargin hmargin]
  (let [rsiz (count img)
        csiz (-> img first count)
        padrow (-> hmargin (* 2) (+ csiz) 
                   (repeat fill) ((partial apply str)))
        vpad (->> padrow (repeat vmargin))
        hpad (->> fill (repeat hmargin))
        body (for [i (range rsiz)]
               (-> hpad (concat (nth img i) hpad)
                   ((partial apply str))))]
    (->> vpad
         (concat vpad body)
         (join "\n"))))

(defn show [img fill vmargin hmargin]
  (-> img 
      (->str fill vmargin hmargin)
      println))
