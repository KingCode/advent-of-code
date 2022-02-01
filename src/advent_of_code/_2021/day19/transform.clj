(ns advent-of-code.-2021.day19.transform
  (:require [advent-of-code.util.-3d-transform :as t]))


(defn make-rotation 
  [rot-f f axis period]
  (if (= 0 period)
    (fn [loc] (f loc)) 
    (fn [loc]
      (rot-f axis period (f loc)))))

(def rperiod
  {1 3 2 2 3 1 0 0})

(defn x-rot [xf] 
  (fn 
    ([f] (partial make-rotation xf f :x))
    ([] (partial make-rotation xf identity :x))))

(defn make-transforms [xf]
  (let [x-rot (x-rot xf)
        x-pos (->> (range 4) 
                   (map (x-rot)))
        rx-pos (->> (range 4) (map rperiod)
                    (map (x-rot)))
        x-neg (->> (range 4)
                   (map  (x-rot (partial xf :y 2))))
        rx-neg (->> (range 4) (map rperiod)
                    (map #(comp (partial xf :y 2)
                                ((x-rot) %))))

        y-pos (->> (range 4)
                   (map (x-rot (partial xf :z 3))))
        ry-pos (->> (range 4) (map rperiod)
                    (map #(comp (partial xf :z 1)
                                ((x-rot) %))))
        y-neg (->> (range 4)
                   (map (x-rot (fn [loc]
                                 (->> loc (xf :z 3)
                                      (xf :y 2))))))
        ry-neg (->> (range 4) (map rperiod)
                    (map #(comp (fn [loc] 
                                  (->> loc (xf :y 2)
                                       (xf :z 1)))
                                ((x-rot) %))))

        z-pos (->> (range 4)
                   (map (x-rot (partial xf :y 1))))
        rz-pos (->> (range 4) (map rperiod)
                    (map #(comp (partial xf :y 3)
                                ((x-rot) %))))
        z-neg (->> (range 4)
                   (map (x-rot (fn [loc]
                                 (->> loc (xf :y 1)
                                      (xf :y 2))))))
        rz-neg (->> (range 4) (map rperiod)
                    (map #(comp (fn [loc]
                                  (->> loc (xf :y 2)
                                       (xf :y 3)))
                                ((x-rot) %))))]
    [(vec (concat x-pos x-neg y-pos y-neg z-pos z-neg))
     (vec (concat rx-pos rx-neg ry-pos ry-neg rz-pos rz-neg))]))


(defn sym [pfx & [sfx]]
  (symbol (str pfx sfx)))

(defn xform-syms 
  ([xyz-pfxs]
   (xform-syms xyz-pfxs nil))
  ([xyz-pfxs sfx]
   (for [axis xyz-pfxs
         sign ['-pos '-neg]
         period (range 4)]
     (symbol (str axis sign "-" period sfx)))))

(defn defsingles
  ([xfs rxfs & [point?]]
   (let [sfx (when point? "-point")
         syms (xform-syms ['x 'y 'z] sfx)
         syms+xfs (map vector syms xfs)
         rsyms (xform-syms ['rx 'ry 'rz] sfx)
         rsyms+rxfs (map vector rsyms rxfs)]
     (doseq [[sym f] syms+xfs]
       (eval `(def ~sym ~f)))
     (doseq [[sym f] rsyms+rxfs]
       (eval `(def ~sym ~f))))))

(let [[xf-1 rxf-1] (make-transforms t/rotate-1)
      [xf rxf] (make-transforms t/rotate)]
  (def transforms-1 xf-1)
  (def r-transforms-1 rxf-1)
  (def transforms xf)
  (def r-transforms rxf)
  (defsingles xf-1 rxf-1 :point)
  (defsingles xf rxf))

(defn transform [coll]
  (->> transforms
       (map #(% coll))))

(defn transform-1 [p]
  (->> transforms-1 
       (map #(% p))))

(defn transform-n [n coll]
  ((-> transforms (nth n)) coll))

(defn reverse-transform [n coll]
  ((-> r-transforms (nth n)) coll))

(defn reverse-transform-1 [n point]
  ((-> r-transforms-1 (nth n)) point))

