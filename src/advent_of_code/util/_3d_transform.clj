(ns advent-of-code.util.-3d-transform)

;; trigonometric values for 0, 90, 180, 270... degrees positive angles
(def coses (cycle [1 0 -1 0]))
(def sines (cycle [0 1 0 -1]))

(defn sin [period]
  (nth sines period))

(defn sin- [period]
  (* -1 (sin period)))

(defn cos [period]
  (nth coses period))

(defn cos- [period]
  (* -1 (cos period)))

;; From:
;; https://en.wikipedia.org/wiki/Rotation_matrix#Basic_rotations
#_(def rotations-3D {:x [[1  0    0   ]
                       [0 cos1 sin1-]
                       [0 sin1 cos1]]
                   
                   :y [[cos1  0 sin1]
                       [0     1  0  ]
                       [sin1- 0 cos1]]
                   
                   :z [[cos1 sin1- 0]
                       [sin1 cos1  0]
                       [0     0    1]]})

(defn dot [row col]
  (->> col (map * row) (reduce +)))

(defn transform-with [mat p]
  (mapv #(dot % p) mat))


(defn rotate-x 
  "Rotates points by p positive 90 degree rotations around the x axis."
  [p point]
  (->> point (transform-with [[1   0         0]
                              [0 (cos p) (sin- p)]
                              [0 (sin p) (cos p)]])))

(defn rotate-y
  "Rotates points by p positive 90 degree rotations around the y axis."
 [p point]
  (->> point (transform-with [[(cos p)  0 (sin p)]
                              [   0     1    0   ]
                              [(sin- p) 0 (cos p)]])))


(defn rotate-z
  "Rotates points by p positive 90 degree rotations around the z axis."
[p point]
  (->> point (transform-with [[(cos p) (sin- p) 0]
                              [(sin p) (cos p)  0]
                              [   0      0      1]])))


(def rotates {:x rotate-x :y rotate-y :z rotate-z})

(defn rotate-1 
  [axis period point]
  ((rotates axis) period point))

(defn rotate 
  [axis period points]
  (-> rotates axis 
      (partial period)
      (map points)))



