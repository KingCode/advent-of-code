(ns advent-of-code.-2021.day17.util)

(defn count-pred [pred p1 p2]
  (pred (count p1) (count p2)))

(defn pfx= [p1 p2]
  (= p1 (take (count p1) p2)))

(defn subpath->? [p1 p2]
  (and (count-pred <= p1 p2)
       (pfx= p1 p2)))

(defn <-subpath? [p1 p2]
  (and (count-pred <= p2 p1)
       (pfx= p2 p1)))

(defn subpath<->? [p1 p2]
  (or (subpath->? p1 p2)
      (subpath->? p2 p1)))

(defn suffix [p1 p2]
  (->> p2 (drop (count p1))))

(defn position [[[x _] [y _]]]
  [x y])

(defn trajectory-to-target 
  "Yields a trajectory up to one point past the target"
  ([steps {{xlo 0} :x {xhi 1} :x
           {ylo 0} :y {yhi 1} :y :as tgt}]
   (trajectory-to-target steps xhi ylo))
  ([steps xhi ylo]
   (let [cover 
         (->> steps
              (sequence
               (comp (map position)
                     (take-while (fn [[x y]]
                                   (and (<= x xhi) (<= ylo y)))))))
         end (->> steps (suffix cover) (take 1) (map position))]
     (concat cover end))))

(defn reach
  "Yields the location closest to [xlo yhi] from the origin"
  ([steps 
     {{xhi 1} :x {ylo 0} :y}]
   (reach steps xhi ylo))

  ([steps xhi ylo]
   (-> steps
        (trajectory-to-target xhi ylo)
        reverse 
        second)))

(defn hit? 
  "Returns true if a step exists within a target area, false otherwise"
  ([steps 
     {{xlo 0} :x {xhi 1} :x
      {ylo 0} :y {yhi 1} :y}]
   (hit? steps xlo xhi ylo yhi))

  ([steps xlo xhi ylo yhi]
   (->> (trajectory-to-target steps xhi ylo)
        reverse
        (some (fn [[x y]]
                (and (<= xlo x xhi)
                     (<= ylo y yhi)))))))
