(ns advent-of-code.util.input
  (:require [clojure.string :refer [split] :as s]))


(defn fmt 
  ([dat]
   (fmt dat identity))
  ([dat f]
   (fmt dat "\n" f))
  ([dat outer-sep f]
   (->> (split dat (re-pattern outer-sep))
        (mapv f)))
  ([dat outer-sep inner-sep f]
   (fmt outer-sep identity inner-sep f))
  ([dat outer-sep outer-f inner-sep inner-f]
   (->>
    (split dat (re-pattern outer-sep))
    outer-f
    (mapv (fn [inner]
            (->>
             (split inner (re-pattern inner-sep))
             inner-f))))))
