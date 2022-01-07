(ns advent-of-code.util.sort
  (:require [advent-of-code.util.cond-let :refer [cond-let]]))

(defn keys-comparator 
"A multi-key comparator with arbitrary functions.

 Yields a function taking two arguments x and y and, starting
 with f invokes (compare (f x) (f y)) on each argument function
 until a non-zero result is obtained and returns the first   
 such result, or zero.
"
  ([fs]
   (apply keys-comparator fs))
  ([f & fs]
   (fn [x y]
     (let [csort (fn [fs]
                   (cond-let 
                    (empty? fs) :>> 
                    0
                    (zero? res) [f (first fs) 
                                 res (compare (f x) (f y))]
                    (recur (rest fs))
                    :else 
                    res))]
       (csort (cons f fs))))))
