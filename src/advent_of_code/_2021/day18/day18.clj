(ns advent-of-code.-2021.day18.day18
  (:refer-clojure :exclude [next remove replace]
                  :rename {reduce core-reduce})
  (:require [advent-of-code.-2021.day18.input
             :refer [sample1 sample2 sample2-sol sample2-sol-intermediate-results
                     sample3-step1 sample3-step2 sample3-step3 sample3-step4
                     sample3-step5 tiny-sample1 tiny-sample1-sol
                     tiny-sample2 tiny-sample2-sol tiny-sample3 tiny-sample3-sol
                     magnitude-samples+sols
                     homework-mag homework-sample homework-sol
                     homework-high-magnitude-sol input]]
            [clojure.zip :as z 
             :refer [down end? left next prev replace right root up vector-zip]]
            [advent-of-code.util.cond-let 
             :refer [cond-let]]))

;; The crux of this challenge resides in the need to access, merge and modify
;; structurally un-related leaves of a randomly nested tree.

;; It would be nice to have a way to flatten the tree in order to quickly access
;; the required leave, and then be able to rebuild the structure...for example,
;; be able to separate the structure from the content.

;; Let's explore by performing a sample explode operation the hacker way:

(let [snum [[[[[9 8] 1] 2] 3] 4]
      pair-idxs [0 0 0 0]
      [lval, rval :as pair] (get-in snum pair-idxs)
      rarg-idxs [0 0 0 1]
      rarg (get-in snum rarg-idxs)
      rarg' (+ rval rarg)]
  (-> snum 
      (assoc-in pair-idxs 0)
      (assoc-in rarg-idxs rarg')))
;;=> [[[[0 9] 2] 3] 4]

;; Thanks to assoc-in, the update is easy if we can find a way to collect indexes.

;; By walking the tree up/down/sideways, we can collect composite indexes
;; along the way. Also, the navigation should be straight-forward if consistent,
;; such as in a depth-first walk, as seems to be the case here...enter the mighty
;; clojure zipper. 
;;
;; ...After doing some exploratory work (see ns day18/explore), it turns out that
;; the zipper a good tool for the job. However, mixing indexes and 
;; index-agnostic techonologies causes a "crossing chasms". The indexes are easy 
;; enough to figure out when we start at the top (zipper roo). However the index
;; of the next node in a depth-first traversal is another matter entirely, because
;; its level may not relative to the current node other than known direct neighbours
;; (sidewyas up or down), illustrated by the search for the closest number 
;; through layers of nested containers. 
;; 
;; So avoid it is much better if using the zipper API, to rely on it entirely.
;; For our purpose, the special sauce is made of the ubiquitous `prev` and
;; `next` functions, which allow travelling from end to end without having to 
;; pay any concern to the specific structure, other than go left (prev) or
;; right (next). For the 4-nested test we simply look at the number of parent
;; nodes in the path of the zipper location. 

;; The only maintenance required while zipper-traveling is the need to remember
;; the distance travelled between some nodes of interest. Specifically, after 
;; updating the closest left number, the exact same path in the opposite direction
;; must be used to travel to the right number on the other side, via the  
;; exploded location.

(defn atomic-pair? [v]
  (and (coll? v)
       (= 2 (count v))
       (number? (first v))
       (number? (second v))))

(defn nav [z move found? yield & [with-distance?]]
  (loop [z z dist 0]
    (cond 
      (or (nil? z) (end? z))
      nil

      (found? z)
      (if with-distance? 
        [(yield z) dist]
        (yield z))

      :else
      (recur (move z) (inc dist)))))

(defn nested-pair? [[v {:keys [pnodes] :as path}]]
  (and (atomic-pair? v)
       (= 4 (count pnodes))))

(defn nested-loc [z]
  (nav z next nested-pair? identity))

(defn number-loc? [z]
  (-> z first number?))

(defn left-number-loc [z & [with-distance?]]
  (nav z prev number-loc? identity with-distance?))

(defn right-number-loc [z & [with-distance?]]
  (nav z next number-loc? identity with-distance?))

(defn n-steps [z n move]
  (loop [z z n n]
    (if (zero? n) 
        z
        (recur (move z) (dec n)))))

(defn do-explode [z]
  (if-let [[[larg rarg :as pair] :as ploc] (nested-loc z)]
    (let [nopair (-> ploc (replace 0))
         [[left-number :as l-loc] l-steps] (-> nopair prev 
                                               (left-number-loc :+steps))
         l' (when left-number 
              (+ left-number larg))
         [[right-number :as r-loc] r-steps] (-> nopair next 
                                            (right-number-loc :+steps))
         r' (when right-number 
              (+ right-number rarg))]
      (cond 
        (and left-number
             right-number)
        (-> l-loc (replace l')
            (n-steps (inc l-steps) next)
            (n-steps (inc r-steps) next)
            (replace r'))

        left-number
        (-> l-loc (replace l'))

        right-number
        (-> r-loc (replace r'))
        
        :else
        nopair))
    z))

(defn split-1 [x]
  (let [l (quot x 2)
        r (if (= x (* 2 l))
            l
            (inc l))]
    [l r]))

(defn unsplit-number? [[v _]]
  (and (number? v) (< 9 v)))

(defn do-split [z]
  (if-let [[v :as z'] (-> z (nav next unsplit-number? identity))]
    (-> z' (replace (split-1 v)))
    z))

;; This may be faster on shallow large zippers,
;; than (-> z root vector-zip)
(defn rewind [[_ path :as z]]
  (if path
    (recur (up z))
    z))

(defn reduce [snum & [use-rewind? is-zipped?]]
  (loop [z (if is-zipped? 
             snum 
             (vector-zip snum)), old-z nil]
    (cond-let
      (= old-z z)
      :>>
      (if is-zipped? z (root z))

      (not= z exploded-z)
      [exploded-z (do-explode z)]
      (if (or use-rewind? is-zipped?)
        (recur (-> exploded-z rewind) z)
        (recur (-> exploded-z root vector-zip) z))

      (not= z split-z)
      [split-z (do-split z)]
      (if (or use-rewind? is-zipped?)
        (recur (-> split-z rewind) z)
        (recur (-> split-z root vector-zip) z))
      
      :else
      (recur z z))))

(defn sum [snums & [use-rewind? is-zipped?]]
  (->> snums rest
       (core-reduce (fn [acc-num snum]
                      (reduce [acc-num snum] use-rewind? is-zipped?))
                    (first snums))))

(defn magnitude [[l r :as snum]]
  (case [(number? l) (number? r)]
    [true true]
    (+ (* 3 l) (* 2 r))
    
    [true false]
    (+ (* 3 l) (* 2 (magnitude r)))

    [false true]
    (+ (* 3 (magnitude l)) (* 2 r))
    
    (magnitude [(magnitude l) (magnitude r)])))


(defn answer1 [input & [use-rewind?]]
  (-> input (sum use-rewind?) magnitude))

;; (answer1 homework-sample)
;;=> 4140
(= homework-mag (answer1 homework-sample))
;;=> true

;; The input runs a little faster using our `rewind` fn above
;; as opposed to creating a new zipper from scratch:

;; (time (answer1 input))
;;=> "Elapsed time: 1217.708726 msecs"
;;   3305

;; (time (answer1 input :rewind))
;;=> "Elapsed time: 1131.813889 msecs"
;;   3305


;;; PART 2
;;

;; A combinatorial version of Part 1. Each 2-combination of
;; the input  is reduced and the highest magnitude is the result.

;; In part 1, a new zipper was created for each reduce operation.
;; A new option has been added to use a single zipper per snailfish 
;; number throughout, to see if it might improve performance.

(defn zip-magnitude [z]
  (magnitude (root z)))

(defn by-pairs [input]
  (let [input (vec input)
        k (count input)]
    (->>
     (for [i (range k)
           j (range k)
           :when (< i j)]
       [i j])
     (sequence
      (comp
       (mapcat (fn [[i j]]
                 [[i j] [j i]]))
       (map (fn [pair] 
              (->> pair (mapv #(nth input %))))))))))


(defn answer2 [input & [use-rewind? zip-it?]]
  (->> input by-pairs
       (sequence
        (comp
         (map #(if zip-it?
                 (vector-zip %)
                 %))
         (map #(reduce % use-rewind? zip-it?))
         (map (if zip-it? 
                zip-magnitude
                magnitude))))
       (apply max)))


(= homework-high-magnitude-sol (answer2 homework-sample))
;;=> true

;; It looks like the zipper/rewind optimizations weren't worth it after all,
;; with only 2% improvement at most. 


;; (time (answer2 input))
;;=> "Elapsed time: 19095.002024 msecs"
;;  4563

;; (time (answer2 input :rewind))
;;=> "Elapsed time: 19102.947455 msecs"
;; 4563

;; (time (answer2 input :rewind :keep-zipped))
;; "Elapsed time: 18888.508435 msecs"
;; 4563

;; This makes sense after all, as persistent data structures' updates (including 
;; zippes) even though fast, yield a new instance after each update. 
;; To go faster, one probably would need one with in-place mutations.
     
;; We can also parallelize whenever possible, e.g. with `pmap`: 

(defn answer2-concurrent [input & [use-rewind? zip-it?]]
  (->> input by-pairs
       (pmap (fn [pair]
               (-> (if zip-it?
                      (vector-zip pair)
                      pair)
                    (reduce use-rewind? zip-it?)
                    (#((if zip-it?
                         zip-magnitude
                         magnitude) %)))))
       (apply max)))

;; (time (answer2-concurrent input))
;;=> "Elapsed time: 7043.604367 msecs"
;;  4563

;; (time (answer2-concurrent input :rewind))
;; "Elapsed time: 6843.044459 msecs"
;; 4563

;; (time (answer2-concurrent input :rewind :zip))
;;=> "Elapsed time: 6912.310279 msecs"
;;  4563


;; That is a much better choice - almost a 2/3 speed-up. 
