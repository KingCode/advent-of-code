(ns advent-of-code.util.scratch
  (:require [advent-of-code.util.cond-let :refer :all]))


(defn cond-let-recur [n a]
  (loop [flag 10]
    (cond-let 
     (neg? flag) :>> :ended
     (:stop @a) :>> :stopped
     (odd? x) [x n _ (println :3)] (recur (inc x))
     (< n 10) [y (inc n) _ (println :4)] (recur y)
     :else [f flag _ (println :ELSE )] (recur (dec f)))
))

(defn test-cond-let-recur [n]
  (def a (atom {:stopped nil}))
  (def stop (fn [] (reset! a {:stop true})))
  (cond-let-recur n a))

