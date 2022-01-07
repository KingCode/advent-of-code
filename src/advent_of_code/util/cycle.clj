(ns advent-of-code.util.cycle
  (:refer-clojure :exclude [get] :rename {nth core-nth
                                          next core-next
                                          rest core-rest
                                          first core-first}))

(defprotocol CycleMemo 
  (get [_] "Yields the underlying cycle")
  (pattern [_] "Yields the prefix sequence of all non-repeating elements")
  (nth [_ n])
  (rest [_])
  (first [_])
  (next [_]))


;;; TODO deftype properly implementing CycleMemo without :refer-clojure renames 

;; this doesn't work, defmethod complains of class not found 
#_(defmethod print-method advent-of-code.util.cycle.CycleMemo
  [cm, w]
  (let [len (-> cm pattern count)]
    (print-method "cm<" w)  
    (print-method (take len (get cm)) w)
    (print-method "...>" w)))


(defn memoed-cycle [xs]
  (let [c (atom (cycle xs))
        value-and-set (fn 
                        ([] (@c))
                        ([f & args] (apply swap! c f args)))]
    (reify CycleMemo
      (get [_] @c)
      (pattern [_] xs)
      (nth [_ n] (core-nth @c n))
      (rest [_] (value-and-set core-rest))
      (next [_] (value-and-set core-next))
      (first [_] (core-first @c)))))



