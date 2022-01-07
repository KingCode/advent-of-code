(ns advent-of-code.util.queue
(:import [clojure.lang Counted IPersistentVector IPersistentStack Seqable]))

;; From Joy of Clojure
(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))


(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))


#_(deftype Queue [q]
    Counted
    (count [_] (count q))

   
    IPersistentStack 
    (seq [_] (seq q))
    (empty [_] (empty q))
    (pop [_] (pop q))
    (peek [_] (peek q))
    (cons [_ x] (conj q x))
)

;; (declare filtered-queue)

#_(deftype FilteredQueue [q pred on-cons on-pop]
  Counted
  (count [_] (count q))
  
  IPersistentStack
  (seq [_] (seq q))
  (empty [_] (empty q))
  (pop [_] (let [popped (peek q)]
             (filtered-queue (pop q) 
                             (on-pop q pred))))
  (cons [this x] (if (pred x) 
                   (filtered)
                   ())))

#_(defn filtered-queue [xs pred on-cons on-pop]
  (->FilteredQueue xs pred on-conj on-pop))
;; (deftype DistinctQueue )
#_(defn queue-nodupes 
  ([]
   (queue-nodupes []))
  ([coll]
   (->DistinctQueue (queue coll) #{})))
