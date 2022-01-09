(ns advent-of-code.-2021.day17.day17
  (:require [advent-of-code.-2021.day17.input 
             :refer [sample input
                     sample-6-3-trajectory
                     sample-9-0-trajectory
                     sample-7-2-trajectory
                     sample-initial-velocities]]

            [advent-of-code.-2021.day17.util
             :refer [hit? position trajectory-to-target
                     <-subpath?]]
            [advent-of-code.-2021.day17.display 
             :as d]
            [clojure.math.combinatorics :as combo]))

;; The first task is to write a projectile motion 
;; generator and related utilities to help understand how 
;; projectiles work (see ns day17/util, display):

(defn next-x-velocity [velo]
  (cond 
    (pos? velo) (dec velo)
    (neg? velo) (inc velo)
    :else velo))

(defn next-x [x velo]
  [(+ x velo) (next-x-velocity velo)])

(defn next-step [[[x xvelo] [y yvelo]]]
  [[(+ x xvelo) (next-x-velocity xvelo)]
   [(+ y yvelo) (dec yvelo)]])

(defn steps [x-velo y-velo]
  (iterate next-step [[0 x-velo] [0 y-velo]]))

(->> (steps 7 2) (take 8) (map position) (= sample-7-2-trajectory))
;;=> true
(-> (steps 7 2) (trajectory-to-target sample) (<-subpath? sample-7-2-trajectory))
;;=> true
(-> (steps 6 3) (trajectory-to-target sample) (<-subpath? sample-6-3-trajectory))
;;=> true
(-> (steps 9 0) (trajectory-to-target sample) (<-subpath? sample-9-0-trajectory))
;;=> true

(hit? (steps 7 2) sample)
;;=> true
(hit? (steps 6 3) sample)
;;=> true
(hit? (steps 9 0) sample)
;;=> true

(not (hit? (steps 17 -4) sample))
;;=> true

(hit? (steps 6 9) sample)
;;=> true

(defn show 
  ([vx vy target]
   (show vx vy target 0))
  ([vx vy target margin]
   (d/show 
    (trajectory-to-target (steps vx vy) target) 
    target :margin margin)))

(defn ->config [xlo xhi ylo yhi]
  {:x [xlo xhi] :y [ylo yhi]})

;; And now the hard part. This is a projectile motion emulation,
;; documented here:

;; https://en.wikipedia.org/wiki/Projectile_motion#Kinematic_quantities_of_projectile_motion

;; From the problem statement and diagramming, the following can be observed
;; when the initial y velocity vy0 is positive:
;;
;; 1) In order to reach the maximum height, the initial y velocity vy0 must be 
;;   positive, e.g. even though intial velocities (10 -1) are successful,
;;
;;            (show 10 -1 sample 5) 
;;
;;   the possible max height of 45 is not reached, as if we use instead 
;;   for example (7 9):
;;
;;            (show 7 9 sample 5) 
;;
;; 2) Multiple paths can reach the maximum height, e.g. in the example at least
;;   (6 9) (7 9) reach the target
;;
;; 3) After a certain time the horizontal displacement doesn't change, i.e.
;;   the projectile falls vertically (vx decrements then stays at zero). 
;;   So vx must have a minimum value no matter the value of vy: since we know
;;   the target value's left border xLow we have vx0 s.t.
;;       vx0 + vx1 + ... + 0 >= xLow  =>
;;       vx0 + (vx0-1) + (vx0-2) + ... + 0 >= xLow =>
;;       (vx0 . (vx0 + 1)) / 2 >= xLow    
;;
;; 4) The maximum height is reached when vy becomes zero 
;;    vy = 0 at time t[max-height]
;;
;; 5) the y position touches the x axis twice, last on its way back down before  
;;   hitting the target. This makes sense, since the velocity decreases upward
;;   then increases downward by the same amount.
;;
;; 6) More importantly, the highest speed occurs when the bottom of the target 
;;   is reached. And in turn, the higher the drop the greater the speed. 
;;   So the maximum height will cause the bottom of the target to be hit
;;   (any greater height causes a miss). Conversely, a lower height may still
;;   hit the target bottom, but will create at least one point between the x-axis
;;   and the bottom, e.g.  velocities (6 3) and (6 4) in the example:  
;;
;;      (show 6 3 sample 5)
;;      (show 6 4 sample 5) 
;;   
;;  The insight leads us to conjecture that the maximum height can be reached
;;  only when vy becomes |yLow| upon crossing the x-axis. Furthermore, we can
;;  backtrack the steps leading to the target bottom, all the way to the step
;;  where vy = 0 in order to find the peak. Since we are interested in the 
;;  distance between the x-axis and maxY, we add up the steps above x-axis:
;; 
;;     base = |yLow| - 1 (from y-velocity rules)
;;     maxY = base + (base-1) + (base-2) + ... + (base - base)
;;     => maxY = base.(base+1)/2  

(defn abs [x]
  (if (pos? x) x (- x)))

(defn answer1 [{{ylo 0} :y :as input}]
  (let [base (-> ylo abs dec)]
    (-> base (* (inc base)) (/ 2))))


(answer1 sample)
;;=> 45

(answer1 input)
;;=> 3916


;;;;;; PART 2 
;;
;; An initial impulse is to compare the number of successful v0 values
;; to the target area: since 10 x 5 = 50 is way below 112, we know that 
;; many different paths will hit the target in the same locations.

(count sample-initial-velocities)
;;=> 112

(->> sample-initial-velocities 
     (every? (fn [[vx vy]] 
               (-> (steps vx vy) 
                   (hit? sample)))))
;;=> true

;; What about the velocities' range?
(defn vxs [velos]
  (->> velos (map first) distinct))

(defn observed-vrange [velos]
  (let [min+max (juxt #(apply min %) #(apply max %))
        vx-range (vxs velos) 
        vy-range (->> velos (map second))]
    [(min+max vx-range) (min+max vy-range)]))

(-> sample-initial-velocities observed-vrange)
;; [[6 30] [-10 9]]

;; So in the samples 6 <= vx <= 30 and -10 <= vy <= 9

;; Now let's look at them in sorting order:
;;
(defn get-vys [sorted-velos vx]
  (let [vx= (fn [[vx-kand vy]]
              (= vx vx-kand))] 
    (->> sorted-velos 
         (drop-while (complement vx=))
         (take-while vx=)
         (map second))))

(defn show-vrange [velos]
  (let [velos (sort velos)
        vxs (vxs velos)
        vxvys (fn [vx]
                [vx (get-vys velos vx)])]

    (->> vxs
         (map vxvys)
         (#(doseq [vxvys %] (apply println vxvys))))))

;; (show-vrange sample-initial-velocities)
;;=>
;;       6 (0 1 2 3 4 5 6 7 8 9)
;;       7 (-1 0 1 2 3 4 5 6 7 8 9)
;;       8 (-2 -1 0 1)
;;       9 (-2 -1 0)
;;       10 (-2 -1)
;;       11 (-4 -3 -2 -1)
;;       12 (-4 -3 -2)
;;       13 (-4 -3 -2)
;;       14 (-4 -3 -2)
;;       15 (-4 -3 -2)
;;       20 (-10 -9 -8 -7 -6 -5)
;;       21 (-10 -9 -8 -7 -6 -5)
;;       22 (-10 -9 -8 -7 -6 -5)
;;       23 (-10 -9 -8 -7 -6 -5)
;;       24 (-10 -9 -8 -7 -6 -5)
;;       25 (-10 -9 -8 -7 -6 -5)
;;       26 (-10 -9 -8 -7 -6 -5)
;;       27 (-10 -9 -8 -7 -6 -5)
;;       28 (-10 -9 -8 -7 -6 -5)
;;       29 (-10 -9 -8 -7 -6 -5)
;;       30 (-10 -9 -8 -7 -6 -5)

;; We can see that the vx coordinates mostly follow each other in a couple  
;; of cluster when ordered: 6..15 and 20..30
;; The vy coordinates are always in a single, tight range for each vx, but
;; the range changes often, depending on the vx. Intuitively, the higher 
;; the vx, the lower the corresponding vy in order not to overshoot; starting with
;; vx = 10 aiming down is necessary.
;;
;; Can we determine the velocity ranges directly from the input? This would allow
;; us to consider a somewhat managed combinatorial (brute-force) approach on that
;; limited range. Before going further, let's see if it is worth it at least on 
;; the exmple: 
;;
(defn ->range [lo hi]
  (range lo (inc hi)))

(defn complexity [[[xlo xhi] [ylo yhi] :as vrange]]
  (->> [(->range xlo xhi) (->range ylo yhi)]
       (apply combo/cartesian-product) count))

(complexity (observed-vrange sample-initial-velocities))
;;=> 500

;; 500 candidates should be doable since we don't need very many steps of each
;; to determine its success/failure.

(defn brute-force-hitcount [[[xlo xhi] [ylo yhi] :as vrange] target]
  (->> [(->range xlo xhi) (->range ylo yhi)] 
       (apply combo/cartesian-product)
       (filter (fn [[vx vy]]
                 (hit? (steps vx vy) target)))
       count))

(time (brute-force-hitcount (observed-vrange sample-initial-velocities) sample))
;;=> "Elapsed time: 5.99749 msecs"
;;   112

;; Reasonable, so let's try to find out a way to deduce the velocity raange from
;; the target...
;;
;; We already know vy's upper bound from part 1 and from observation 6). 
;; For vy's lower bound, we observe in the example that it is the same as 
;; the value of the bottom border of the target - can we generalize?
;;  
;; CLAIM: vyO must be at least yLow.
;; Proof: Suppose vy0 < yLow. 
;;        Then as the velocity increases negatively after each step,
;;        the first step's velocity will cause its location to be below 
;;        the target bottom, a contradiction.
;; 
;; A corollary to vy0's lower bound is that velocities using yLow must have 
;; a vx large enough to reach the target on the first step.
;;
;; For the vx0 boundaries, the upper boundary is obviously xHi, the right 
;; target border, or the first step will overshoot.
;;
;; For the lower vx0 boundary, we have seen in observation 3) above that if vy0
;; is positive vx0 must be large enough to align (or pass) the left border wall 
;; when the projectile crosses the x-axis, so we determine vx0 from it:
;;      (vx0 . (vx0+1)) / 2 = xLow =>
;;      vx0^2 + vx0 = 2 . xLow  =>
;;      a^2 + a + (-2.xLow) = 0  where a = vx0 
;;
;; and we have a solvable quadratic equation:
;;   vx0 = a = [-1 +/- sqrt(1 - 4.(-2.xLow))]/2
;;
;; which we round up to the next integer.
;;
;; Can we use this also for downward, negative vy0? Yes, because the x-motion is
;; independant of vy0: vx must be large enough to make it to the west border 
;; before it becomes zero.

(defn lowest-vx0 [{{xlo 0} :x :as tgt}]
  (let [c (* -2 xlo)
        right-term (-> 1 (- (* 4 1 c))
                       (#(Math/sqrt %)))
        pos-ntr (-> 1 - (+ right-term))
        neg-ntr (-> 1 - (- right-term))]
    (-> (filter pos? [pos-ntr neg-ntr]) 
        first
        (/ 2) 
        (#(Math/ceil %))
        int)))

(defn vrange [{{xlo 0} :x {xhi 1} :x
               {ylo 0} :y {yhi 1} :y :as tgt}]
  [[(lowest-vx0 tgt) xhi]
   [ylo (-> ylo abs dec)]])

;; Is this manageable? Let's find out:

(complexity (vrange input))
;;=> 7192

;; ouch! Let's try it in the repl, and be ready to shut it down if running amock..
(defn answer2 [input]
  (-> input vrange (brute-force-hitcount input)))

(answer2 sample)
;;=> 112
(time (answer2 input))
;;=> "Elapsed time: 547.979948 msecs"
;;  2986


;; ....Grooovy!....err, Clojure! This is fast enough and accurate.
