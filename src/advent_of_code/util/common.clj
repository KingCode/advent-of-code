(ns advent-of-code.util.common
  (:require [advent-of-code.util.cond-let :refer [cond-let]]))

(defn abs [x]
  (if (<= 0 x) x (- x)))

(defn cons-all
"Useful for initiating a lazy-seq with cons'es.
 Creates a seq using cons"
  ([xs]
   (cons-all xs nil))
  ([xs more]
   (if (seq xs)
     (cons (first xs) (cons-all (rest xs)))
     more)))

(defn split-after
"Returns a vector of [(take-while pred  coll) (drop-while pred coll)]
"
[pred coll]
  [(take-while pred coll), (drop-while pred coll)])


(defn split-between
  "Returns a vector of [(filter pred coll), (remove pred coll)]"
  [pred coll]
  (reduce (fn [[yes no] x]
            (if (pred x)
              [(conj yes x) no]
              [yes, (conj no x)]))
          [[] []]
          coll))


(defn split-trunk 
  "Merges common leading elements of colls according to (pred xs),
  into a trunk; returns a duple [trunk branches] of merges and branches;
  `pred takes a collection arg."
  ([colls]
   (split-trunk #(apply = %) colls))
  ([pred colls]
   (loop [trunk [] branches colls]
     (if (->> branches (map first) pred)
       (recur (conj trunk (ffirst colls))
              (map rest branches))
       [trunk branches]))))

(defn when? [pred x]
  (when (pred x) x))


(defn ->fn [x-or-fn]
  (if (fn? x-or-fn)
    x-or-fn
    (constantly x-or-fn)))


(defn frequencies-by
  "Returns a map from distinct results of (f item) in coll 
  to a vector tuple of all (k item) (if k is provided, or items otherwise) 
  with the same (f item) value, and the number of times (f item) appears.
"
  ([f coll] 
   (frequencies-by f nil coll))
  ([f k coll] 
   (persistent! ;; copied from clojure.core
    (reduce (fn [counts x]
              (assoc! counts (f x) 
                      (let [[xs kount] (get counts (f x) [[] 0])] 
                        [(conj xs (if k (k x) x)) 
                         (inc kount)])))
            (transient {}) coll))))


(defn every-position? 
"Yields true if binary pred is truthty when applied to two respective elements
  of both xs and ys for each position in idxs"
[pred xs ys idxs]
  (every? #(pred (nth xs %) (nth ys %)) idxs))


(defn positions 
"The converse of `every-position?`. Yields a vector of positions for which 
 (pred x y) is true for respective elements of xs ys in the same position.
 "
[pred xs ys]
  (->> (map vector (range) xs ys)
       (keep (fn [[i x y]]
                 (when (pred x y)
                   i)))
       vec))


(defn sel-positions 
"Yields keys of every x in xs for which there is an element in sel such
 that (pred sel x) is truthy"
[pred sel xs]
  (->> (if (map? xs)
         xs
         (map-indexed vector xs))
       (keep (fn [[k x]]
               (when (some #(pred % x) sel)
                 k)))))

(defn some-position
  "Yields the first position of x in xs if found; otherwise nil is returned"
  [x xs]
  (when-let [idxs (seq (sel-positions = [x] xs))]
    (first idxs)))

(defn sel-vals+positions
"Same as sel-positions, but a tuple of [v, k] is returned for 
 each matching x element, where (pred v x) is truthy for some
 element in sel"
  [pred sel xs]
    (->> (if (map? xs)
         xs
         (map-indexed vector xs))
       (keep (fn [[k x]]
               (when-let [vs (seq (filter #(pred % x) sel))]
                 [(first vs) k])))))


(defn inverse-map 
"m is a one-to-one map. Creates a map of values of m to keys of m" 
[m]
  (into {}
        (map (comp vec reverse) m)))


(defn sliding-windows 
  "Yields a lazy sequence of sliding windows of size n
  from xs"
[n xs]
  (let [slider (fn slide [frame xs]
                 (let [window (when (= n (count frame))
                                (vec (reverse frame)))
                       x (first xs)
                       rxs (rest xs)]
                   (cond 
                     (and window x)
                     (cons window 
                           (slide (->> frame 
                                          (cons x)
                                          (take n))
                                     rxs))
                     
                     window
                     (cons window rxs)

                     x
                     (recur (conj frame x) rxs)

                     :else
                     nil)))]
    (lazy-seq (slider nil xs))))

(defn extract-nths
  "Yields a seq of all non-aggregated (atomic) nth elements from
  a nested sequential coll, starting with the outer collection's
  n'th element, if any, and including all n'th elements of nested 
  collections not in a n'th position within their parent.
  "
 [n coll]
  (let [step 
        (fn f [x acc]
          #_(cond-let 
           (coll? x ))
)]
    (step coll [])))


(defn positions [coll]
  (->> coll
       (into {} (comp 
                 (map-indexed vector)
                 (map reverse)
                 (map vec)))))
