(ns advent-of-code.-2021.day17.display)

(defn grid [path {{xlo 0} :x {xhi 1} :x
                  {ylo 0} :y {yhi 1} :y}
            & {:keys [margin] :as opts}]
  (let [max-x (->> path (map first) (apply max) (max xhi))
        ys (->> path (map second))
        min-y (->> ys (apply min) (min ylo))
        max-y (->> ys (apply max) (max yhi))
        margin (->> \space (repeat (or margin 0))
                    (apply str))
        point? (set path)
        target? (fn [[x y]]
                  (and (<= xlo x xhi)
                       (<= ylo y yhi)))]
    (->>
     (for [y (range max-y (dec min-y) -1)
           x (range (inc max-x))
           
           :let [xtra (when (and (pos? x)
                                 (zero? (mod x max-x)))
                        (str "\n" margin))
                 c (cond 
                     (point? [x y])
                     "#"
                     (target? [x y])
                     "T"
                     :else
                     ".")]]
       (str c xtra))
     (apply str "\n" margin))))

(defn show 
  ([path config & opts]
   (-> (apply grid path config opts) show)
   (println :MAX-HEIGHT 
            (->> path (map second) (apply max))))
  ([grid]
   (println  grid)))
