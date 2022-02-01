(ns advent-of-code.-2021.day19.util)

(defn segment? [[a _]]
  (and (sequential? a) (= 3 (count a))))

(defn points-ordered? [[ax ay az :as a] [bx by bz :as b]]
  (cond 
    (< ax bx) true
    (> ax bx) false
    (< ay by) true
    (> ay by) false
    (< az bz) true
    :else false))

(defn sorted-combo
  "Yields all 2-combinations of coll where each pair [a b] is such that 
  (ordered? a b), if ordered? is provided (defaults to <)"
  [coll & [ordered?]]
  (let [< (or ordered? points-ordered?)]
    (for [a coll
          b coll
          :when (< a b)]
      [a b])))

(defn segments [points]
  (->> points sort 
       sorted-combo sort))


#_(defn psome [f coll]
  (let [result (atom nil)]
    (->> coll 
         (pmap (fn [x]
                 (if @result
                   nil
                   (when-let [y (f x)]
                     (reset! result y)))))
         doall)
    @result))

(defn psome [f coll & [log]]
  (let [res (atom nil)
        done-fut (atom nil)
        futs (->> coll
                  (map #(future 
                          (if @res 
                            nil
                            (when-let [v (f %)]
                              (reset! done-fut f)
                              (reset! res v)))))
                  doall)
        wkey (keyword (str *ns*) (str (gensym "psome-v"))) 
        w (add-watch res wkey
                     (fn [_ _ _ _] 
                       (doseq [f futs] 
                         (when (not (identical? f @done-fut))
                           (future-cancel f)))))]
    ;; ensure all have run or been cancelled
    (->> futs (reduce (fn [_ fut]
                        (when
                            (not (or (future-cancelled? fut)
                                     (future-done? fut)))
                          (try @fut
                               (catch Throwable t "future deref exc caught")))
                        (when log
                          (cond 
                            (future-cancelled? fut) 
                            (log 1 "(duplicate task cancelled)")
                            (future-done? fut)
                            (log 2 "(task completed)")
                            :else
                            (log 2 "(task pending"))))
                      nil))
    (remove-watch w wkey)
    @res))
