(ns advent-of-code.-2021.day20.day20
 (:require [advent-of-code.-2021.day20.input 
            :refer [input sample sample-algo
                    sample-img0 sample-img1 sample-img2 sample-img3]]
           [advent-of-code.util.-2d :as d2]
           [advent-of-code.util.number.base :as b]
           [advent-of-code.-2021.day20.display :refer [show]]
           [clojure.core.reducers :as r]))

;; As the image is infinite, we want a model which deals foremost with
;; accurate representation of pixels outside the bounds of the finite
;; representation (the input, base image or a transformation thereof).
;;
;; Since the base image consists of both lit (#) and unlit (.) pixels,
;; and is always surrounded by unlit pixels, an algorithm which map 
;; 0 to a list pixel will cause an infinite area of dots (made of 3x3
;; crops of dots, or binary zero) to be lit, the model has to accomodate
;; such a scenario. Therefore we need to represent the "hydrated" portion,
;; which has rows and columns of specified pixels, and a "compressed" 
;; portion which specifies a single value for all other pixels. Both of these
;; put together model the infinite image. The input proper specifies the 
;; hydrated portion of the base image. A subsequent transform may expand
;; or retract the hydrated portion, and have an infinity of either lit or
;; unlit pixels.  

#_(defn image [algo visible fill]
  {:algo algo
   :hydrated visible
   :compressed fill})

(defn base-image 
  ([input]
   {:algo (first input)
    :hydrated (last input) 
    :compressed \.})) 

;; When transforming, either a one- or two-pixel thick rectangle
;; around the hydrated portion may be impacted, and require becoming part
;; of the hydrated area upon transformation. 

;; On the other hand, since all pixel assignments are done independently of 
;; one another, each pixel can be processed concurrently.
;; 

(defn pixel->01 
  ([img fill rc]
   (-> img (get-in rc fill) (case \. 0 \# 1))))

(defn crop-bits 
  ([img fill ridx cidx]
   (crop-bits img fill ridx cidx (count img) (count (first img))))
  ([img fill ridx cidx rsiz csiz]
   (->> (d2/clock ridx cidx)
        (into []
              (map (fn [[r c :as rc]]
                     (pixel->01 img fill rc)))))))

(defn expand-boundaries [siz]
  [(range -1 (+ 1 siz)), (+ 2 siz)])

(defn ->2d [colsiz xs]
  (->> xs (partition colsiz) (mapv vec)))

(defn next-pixel-at [[r c] frame fill algo]
  ;; (println :RC [r c] :FRAME frame :FILL fill :ALGO algo)
  (-> frame (crop-bits fill r c) b/binv->dec algo))

;; For now, we'll just use a sequential, single-threaded execution,
;; but leave room for plugging in a faster approach by shelling out
;; the core task to an argument function: we use the infinite image 
;; map to store optional parameters and pass it as is from the shell
;; function:
(defn transform 
  [{:keys [algo] fill :compressed img :hydrated :as inf-img}]
  (let [rsiz (count img)
        csiz (-> img first count)
        [rrange] (expand-boundaries rsiz)
        [crange csiz'] (expand-boundaries csiz)] 
    {:algo algo
     :hydrated
     (->>
      (for [i rrange
            j crange]
        (next-pixel-at [i j] img fill algo))
      (->2d csiz'))
     :compressed (next-pixel-at [-100 -100] nil fill algo)}))

(defn transforms [base-image]
  (iterate transform base-image))

(def ts (transforms (base-image sample)))
(def sh #(show (-> ts (nth %) :hydrated) (-> ts (nth %) :compressed) 5 5))

(defn count-lit-pixels [input n xforms]
  (-> input base-image xforms 
      (nth n) :hydrated flatten
      (#(filter #{\#} %)) count))

(defn answer1 [input]
  (count-lit-pixels input 2 transforms))


;; (time (answer1 sample))
;; "Elapsed time: 1.687631 msecs"
;; 35

;; (time (count-lit-pixels sample 50 transforms))
;; "Elapsed time: 1779.94083 msecs"
;; 3351

;; (time (answer1 input))
;; "Elapsed time: 209.988626 msecs"
;; 5057

;; (/ 1779.94 1.68)
;; 1059.4880952380954

;; Since answer2 takes over a thousand times longer than answer1,
;; we can reasonably expect it to take 209 seconds, or well over 3 minutes
;; for the actual input, but is likely much worse due to the input being much
;; larger. Therefore it is worth looking at caching and parallelization to 
;; mitigate this.

;;                  Caching

;; Since only a single one-pixel thick border gets added during an expansion,
;; we don't need to regenerate the existing row-column indexes:

(defn add-surrounding-indexes
  "rcs is a sorted set; adds row-column indexes for a 1-element perimeter
  to rcs."
  [rcs row-range col-range rsiz csiz]
  (->>
     [(for [j col-range] [-1 j])    ;; added top border
      (for [i row-range] [i csiz])  ;; right
      (for [j col-range] [rsiz j])   ;; bottom
      (for [i row-range] [i -1])]   ;; left
     (into rcs cat)))

(defn add-pos-expansion-indexes
  "rcs is a sortec set of row-column coordinates; adds coordinates for 2 
  columns to the right and 2 rows to the bottom of, rcs"
  [rcs from-rsiz from-csiz to-rsiz to-csiz]
  (->>                      
     ;; right columns to existing rows only
     [(for [i (range from-rsiz) j (range from-csiz to-csiz)] [i j]) 
      ;; bottom, full new rows
      (for [i (range to-rsiz) j (range to-csiz)] [i j])]
     (into rcs cat)))

;;                  Parallelism

;; Finally, we can distribute the pixel assignments to separate threads.
;; Putting this together with caching, let's see how much speedup we get:

(defn next-frame-|| [rcs frame fill algo & [nsplit]]
  (->> rcs
       (r/fold 
        nsplit
        (fn 
          ([] [])
          ([pxs1 pxs2]
           (into pxs1 pxs2)))
        (fn [acc rc]
          (->> (next-pixel-at rc frame fill algo)
               (conj acc))))))

(defn transform2
  ([inf-img]
   (transform2 inf-img next-frame-||))
  ([{:keys [algo opts] frame :hydrated fill :compressed} ->pxs]
   (let [{:keys [rcs rsiz csiz nsplit]}  opts
         ->pxs (or ->pxs next-frame-||)
         rsiz (or rsiz (count frame))
         csiz (or csiz (-> frame first count))
         rcs (or rcs (->> (for [i (range rsiz)
                                j (range csiz)] [i j])
                          (into (sorted-set))))
         nsplit (or nsplit
                    (.. Runtime getRuntime availableProcessors))
         [xrr rsiz'] (expand-boundaries rsiz)
         [xcr csiz'] (expand-boundaries csiz)
         xrcs (add-surrounding-indexes rcs xrr xcr rsiz csiz)
         rcs' (add-pos-expansion-indexes rcs rsiz csiz rsiz' csiz')]
     (->> (->pxs xrcs frame fill algo nsplit)
          (->2d csiz')
          (hash-map :opts {:rsiz rsiz'
                           :csiz csiz'
                           :rcs rcs', :nsplit nsplit}
                    :compressed (next-pixel-at [-100 -100] nil fill algo)
                    :algo algo
                    :hydrated)))))

(defn next-frame-serial [rcs frame fill algo & [_]]
  (->> rcs
       (reduce (fn [acc rc]
                 (->>
                  (next-pixel-at rc frame fill algo)
                  (conj acc)))
               [])))


(defn transforms2 [base-image & [pxf ||size]]
  (->> base-image
       (merge {:opts {:nsplit ||size}})
       (iterate #(transform2 % pxf))))

(defn answer2 
  ([input & [pxf ||size]]
   (count-lit-pixels input 50 #(transforms2 % pxf ||size))))


;; (time (answer2 input nil 50))
;; "Elapsed time: 13935.844749 msecs"
;; 18502

;; Sadly, the optimizations were premature and only added complexity and
;; overhead! The initial version would have been better left alone:

;; (time (count-lit-pixels input 50 transforms))
;; "Elapsed time: 10756.735345 msecs"
;; 18502

;; (comment
  ;; (def ts (transforms (base-image sample)))
  ;; (def ts2 (transforms2 sample next-frame-serial))
  ;; (def ts2f (transforms2 sample next-frame-||))
  ;; (defn h [n t] (-> t (nth n) :hydrated))
  ;; (def h0 #(h % ts))
  ;; (def h1 #(h % ts2))
  ;; (def h2 #(h % ts2f))
  ;; (defn h= [n t & ts] (->> (cons t ts)
                           ;; (map #(-> % (nth n) :hydrated))
                           ;; (apply =)))
  ;; (def h=01 #(h= % ts ts2))
  ;; (def h=12 #(h= % ts2 ts2f))
  ;; (def h=02 #(h= % ts ts2f))
  ;; (def h=all #(h= % ts ts2 ts2f))

  ;; (def sh0 #(show (-> ts (nth %) :hydrated)  
                  ;; (-> ts (nth %) :compressed) 5 5))
  ;; (def sh #(show (-> ts2 (nth %) :hydrated)  
                 ;; (-> ts2 (nth %) :compressed) 5 5))
  ;; (def shf #(show (-> ts2f (nth %) :hydrated)  
                  ;; (-> ts2f (nth %) :compressed) 5 5)))
