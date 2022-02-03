(ns advent-of-code.-2021.day19.day19-better
  (:require [advent-of-code.-2021.day19.input :as i]
            [advent-of-code.-2021.day19.transform :refer [transform]] 
            [advent-of-code.-2021.day19.util :as u]
            [advent-of-code.util.common :refer [abs]]
            [clojure.core.reducers :as r]))

;; From looking at the following site,
;; https://itnext.io/modern-c-in-advent-of-code-day19-ff9525afb2ee

;; a much simpler and also much, much (!) faster algorithm is available,
;; without requiring intra-segment generation and intricate segment
;; pair comparision, as well as checking for multiple corner cases,
;; as used in the previous algorithm.

;;
;; Considering a single dimension and a single point between
;; two scanners,
;;
;;      S...B..S'
;;
;; Assuming both scanners S and S' report B = S[4] = S'[-3],
;; then the distance required to convert B's index from S' to S
;; is 4 - -3 = 7, i.e. the distance between the scanners: we 
;; obtain it with S[B] - S'[B].
;; 
;; The threshold number (12) is used as a pointer to the correct
;; distance, as points only seen from one of the two scanners 
;; must yield different deltas, e.g
;;
;;    A..S...B..S'
;;
;; where S' doesn't report A and A = S[-3] = S'[-10].
;; In this case, since A is not part of S', S[A] can only be 
;; compared to S'[B], i.e.  
;;
;; S[A] - S'[B] = -3 - (-3) = 0, yielding a different value.
;;
;; Therefore in several dimensions, the highest number of shared
;; deltas determine the correct subset of points, and the delta 
;; between scanners. We assume the threshold constraint takes care
;; of ensuring the correct overlap is detected.

(defn delta [a b]
  (mapv - a b))

(defn overlap? [s1 s2 & [threshold]]
  (->>
   (for [a s1
         b s2]
     (delta a b))
   (group-by identity)
   (sequence
    (comp (map second)
          (keep #(when (<= (or threshold 12) (count %)) 
                   (first %)))))
   first))

(defn transforms-from-cache [id locs {:keys [xfs uxfs] :as cache}]
  (if (and id cache)
    (or (xfs id)
        (uxfs id 
               (->> locs transform (map-indexed vector))))
    (->> locs transform (map-indexed vector))))


(defn xf-cache []
  (let [cache (atom {})]
    {:xfs (fn [id]
            (-> @cache (get id)))
     :uxfs (fn [id idx+xfs]
              (-> cache 
                  (swap! assoc id idx+xfs))
             idx+xfs)}))


(defn find-overlap 
  ([s1 s2 & [threshold s2-id {:keys [xfs uxfs] :as cache}]]
   (let [indexed-s2xfs (->> cache 
                            (transforms-from-cache s2-id s2))]
     (->> indexed-s2xfs
          (u/psome 
           (fn [[idx s2t :as i+t]]
             (when-let [<delta (overlap? s1 s2t threshold)]
               [s2t <delta])))))))

(defn find-overlap-with-cache [s1 s2 s2-id cache]
  (find-overlap s1 s2 nil s2-id cache))

(defn grounded-partition 
  "Yields a tuple [grounded ungrounded] from matching ungrounded elements to 
  their suitable anchors, where grounded are the newly grounded elements and
  ungrounded are the unmatched elements unmodified."
[anchors ungrounded & [cache split-count]]
  (->> ungrounded 
       (r/fold 
        (or split-count 10)
        (fn 
          ([] [[] []])
          ([[gs1 ugs1] [gs2 ugs2]]
           [(into gs1 gs2) (into ugs1 ugs2)]))
        (fn [[gs not-gs] [uid ulocs :as u]]
          (if-let [[[t d] anchor-id]
                   (u/psome 
                    (fn [[id [locs]]]
                      ;; (log 1 "Testing for overlap b/w" uid "and anchor" id)
                      (when-let [o (find-overlap-with-cache 
                                    locs ulocs uid cache)]
                        ;; (log 1 "Attaching" uid "to anchor" id)
                        [o id]))
                    anchors)]
            [(conj gs [uid [t d anchor-id]]) not-gs]
            [gs (conj not-gs u)])))))

(defn ->node [locs delta anchor]
  {:locs locs :delta delta :anchor anchor})

(defn into-resolved [resolved anchors]
  (->> anchors 
       (into resolved 
             (map (fn [[id [locs d anchor-id]]]
                    [id (->node locs d anchor-id)])))))

(defn ->graph 
  ([reports]
   (->graph reports nil))
  ([reports ||size]
   (->graph reports :use-cache ||size))
  ([sId+locs use-cache? ||size]
   (let [[s0-id s0-locs] (first sId+locs)
         cache (when use-cache? (xf-cache))]
     (loop [todo (rest sId+locs) 
            prv-todo nil
            anchors [[s0-id [s0-locs]]]
            resolved {}
            ctr 0]
       (cond
         (empty? todo)
         (into-resolved resolved anchors)

         (= todo prv-todo)
         (throw (ex-info "No solution found or progress made" 
                         {:todo-ids (map first todo)
                          :anchor-ids (map first anchors)
                          :resolved resolved}))
         :else
         (let [[gs ungs] (grounded-partition anchors todo cache
                                             (or ||size (count sId+locs)))]
           ;; (log 1 "Resolved" (+ (count resolved)
                                ;; (count anchors)
                                ;; (count gs)) "regions," 
                ;; (count ungs) "to go\n" (apply str (repeat ctr \.)))
           (recur ungs todo gs (into-resolved resolved anchors) (inc ctr))))))))

(defn cumulative-delta [id graph]
  (loop [acc [0 0 0] {:keys [anchor delta]} (graph id)]
    (if (nil? delta)
      acc 
      (recur (mapv + delta acc)
             (graph anchor)))))

(defn assemble [graph]
  (->> graph
       (reduce (fn [acc [id {:keys [locs]}]]
                 (let [d (cumulative-delta id graph)]
                   (->> locs
                        (into acc 
                              (map #(mapv + d %))))))
               #{})))

(defn answer1 [input]
  (-> input (->graph nil) assemble count))


;; (time (answer1 i/input))
;; "Elapsed time: 3539.580975 msecs"
;; 378

;; What a difference an algorithm makes :)

;;;;   PART 2 ;;;;;;;

(defn scanner-positions [g]
  (->> g keys (mapv #(cumulative-delta % g))))

(defn answer2 [input-or-graph & [||size]]
  (let [g (if (map? input-or-graph) 
            input-or-graph
            (-> i/input (->graph ||size)))]
    (->> g 
         scanner-positions
         u/segments
         (map (partial apply delta))
         (map #(mapv abs %))
         (map (partial apply +))
         (apply max))))

;; (time (answer2 i/input))
;; "Elapsed time: 3496.660069 msecs"
;; 13148
