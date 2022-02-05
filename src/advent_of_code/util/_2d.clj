(ns advent-of-code.util.-2d
  "Utilities for operations and queries on 2-dimensional objects")

(def ^:private clock-ops
  (let [ops [dec identity inc]]
    (for [op1 ops 
          op2 ops]
      [op1 op2])))

(defn- remove-identity-pair [ops-pairs]
  (->> ops-pairs (remove #{[identity identity]})))

(def ^:private clock-ops- (->> clock-ops 
                               (remove-identity-pair)))

(def ^:private manhattan-ops
  (->> clock-ops
       (remove (fn [ops]
                 (not-any? #{identity} ops)))))

(def ^:private manhattan-ops- (->> manhattan-ops 
                                   (remove-identity-pair)))

(defn clock 
  "r, c are row and column indexes resp. Yields a lazy-seq of [r c] 
  and all [row col] indexes of all locations around it.
  "
  ([rc]
   (apply clock rc))
  ([r c]
   (->> clock-ops 
        (map (fn [ops]
               [((first ops) r)
                ((second ops) c)])))))

(defn clock-neighbors
  ([rc]
   (apply clock-neighbors rc))
  ([r c]
   (->> clock-ops-
        (map (fn [ops]
               [((first ops) r)
                ((second ops) c)])))))

(defn manhattan
  ([rc]
   (apply manhattan rc))
  ([r c]
   (->> manhattan-ops
        (map (fn [ops]
               [((first ops) r)
                ((second ops) c)])))))

(defn manhattan-neighbors
  ([rc]
   (apply manhattan-neighbors rc))
  ([r c]
   (->> manhattan-ops-
        (map (fn [ops]
               [((first ops) r)
                ((second ops) c)])))))

(defn in-bounds0?
  "Yields wheter [r c] loc is in-bounds in a
  zero-based indexing rectangle, or square if
  using the 2- or 3-arities."
  ([rc size]
   (in-bounds0? (first rc) (second rc) size size))
  ([r c size]
   (in-bounds0? r c size size))
  ([r c rsize csize]
   (and (< -1 r rsize)
        (< -1 c csize))))
