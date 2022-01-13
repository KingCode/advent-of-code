(ns advent-of-code.-2021.day18.explore
  (:require [clojure.zip  :as z
             :refer [up down right left vector-zip]]))

(defn atomic-pair? [v]
  (and (coll? v)
       (= 2 (count v))
       (number? (first v))
       (number? (second v))))


(defn dxs-right [idxs]
  (-> idxs pop (conj (-> idxs peek inc))))

(defn dxs-left [idxs]
  (-> idxs pop (conj (-> idxs peek dec))))

(defn dxs-down [idxs]
  (conj idxs 0))

(defn dxs-up [idxs]
  (when-not (empty? idxs)
    (pop idxs)))

(def dir->fns {:down [down dxs-down]
               :up [up dxs-up]
               :right [right dxs-right]
               :left [left dxs-left]})

(defn next-state
  "z is a zipper, side-f moves z either left or right, and visited? is a set of
  previously visited zipper states.
  Moves z to the next node in a search with priority to depth-first availability 
  and adds it to visited?, and returns a tuple of [new-zipper, updated-visited?,
  updated-history, updated-composite-index]  
  . If down and side directions have been visited, the parent node 
  is returned"
[z side-kw visited? hist idxs]
  (let [actions [(find dir->fns :down) 
                 (find dir->fns side-kw)]
        kand
        (->> actions 
             (keep (fn [[k [zf dxsf]]]
                     (when-let [z' (zf z)]
                       (when (not (visited? z'))
                            [z' side-kw 
                             (conj visited? z')
                             (conj hist k)
                             (dxsf idxs)]))))
             first)]
    (or kand [(up z) side-kw visited? 
              (conj hist :up) 
              (dxs-up idxs)])))

(defn nav [z sel-pred side visited hist idxs]
  (cond
   (or (nil? z) (z/end? z))
   nil 

   (sel-pred z idxs)
   [z idxs]

   :else
   (let [[z' _ v' h' idxs'] (next-state z side visited hist idxs)]
     (recur z' sel-pred side v' h' idxs'))))

(defn explode-kpath [zv]
  (nav zv
       (fn [[v _] idxs]
         (and (= 4 (count idxs))
              (atomic-pair? v)))
       :right #{}[][]))
