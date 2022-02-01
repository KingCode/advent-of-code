(ns advent-of-code.-2021.day19.day19
  (:require [advent-of-code.-2021.day19.input 
             :refer [sample-2d 
                     sample-transforms
                     sample
                     sample-s0s1-0-overlap
                     sample-s0s1-1-overlap
                     sample-s1-0-position
                     sample-s1s4-0-overlap
                     sample-s2-0-position
                     sample-s3-0-position
                     sample-s4-0-position
                     sample-beacons-0-position
                     sample-beacons-0-max-scanner-pos-sId
                     sample-beacons-0-max-scanner-pos 
                     sample-beacons-0-min-scanner-pos-sId 
                     sample-beacons-0-min-scanner-pos
                     sample-max-scanner-manhattan-distance 
                     input]]
            [advent-of-code.-2021.day19.transform :refer [transform]] 
            [advent-of-code.-2021.day19.util :as u]
            [advent-of-code.util.common :refer [abs]]
            [advent-of-code.util.cond-let :refer [cond-let]]
            [advent-of-code.util.logging :as log]
            [clojure.set :refer [difference intersection union]]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [clojure.core.reducers :as r]))

;; (tufte/add-basic-println-handler! {})
(tufte/add-basic-println-handler!
  {:format-pstats-opts {:columns [:n-calls :p50 :p99 :mean :clock :total]
                        :format-id-fn name}})

(defonce logger (log/start-logger 20))
(def ^:dynamic *log-level* 1)
;; (defn log [level & txt]
  ;; (when (<= level *log-level*)
    ;; (apply log/log logger txt)))

(defmacro log [level & txt]
  (when (<= level *log-level*)
    `(log/log logger ~@txt)))

(def ^:dynamic *threshold* 
  "The number of shared points determining an overlap" 12)
(def ^:dynamic *visibility* 
  "The maximum distance for a beacon to be visible from any scanner" 1000)

;; The corner stone of the solution is the ability to identify points (beacons)
;; shared between two coordinate systems, the relative position and orientation of
;; wich is unknown.

;; With the benefit of visualizing the two RHS diagrams we can see that the three
;; beacons have identical relative positions to each other: 
;;
;;     Relative             Unrelated
;;     to one S             S positions 
;;     ...B..        ...B..        ......   
;;     B....S        B.....        ...B..
;;     ....B.        ....B.        B....S
;;     S.....        S.....        ....B.

;; Being told that 3 shared points (in this demo example) makes an overlap, we 
;; start with the assumption that if we have enough 2-point pairs (undirected 
;; segments) whose coordinates' deltas are the same in both systems, then we
;; have an overlap and can deduce and convert the points in the second system
;; into coordinates of the first, using simple "Manhattan distance" arithmetic:
;; we can somehow discover that [0 2] in s0 is equivalent [-5 0] in s1 
;; and so forth. 

;; The relative positions between any two beacons in one coordinate
;; system, e.g. [0 2] and [4 1] in s0 have the same delta between their
;; respective coordinates than [-5 0] and [-1 -1] in s1:
;;        0 - 4 = -5 - -1 = -4 and
;;        2 - 1 = 0 - -1 = 1
;;
;; The s1 coordinates above relative to s0 are the 
;; coordinates of a beacon in s1 subtracted from those in s0:
;;
;;        [0 2] - [-5 0] = [5 2]
;;
;; and therefore a s1 coordinate is converted by adding s1's position to all of
;; s1's coordinates:
;;        [-5 0] + [5 2] = [0 2]
;;        [-1 -1] -> [4 1] 
;;        [-2 1]  -> [3 3]
;;
;; Once an equivalence is found between two pairs of beacons, one from each 
;; systems, and the process repeated for a number of segments yielding a number
;; of equivalent points determined by a threshold number, we need to ask the
;; question: could these be points that are only equivalent and not shared?

;; Assuming a visibility of 6 and a threshold of 3 points for an overlap,
;; the next diagram shows that the correct overlaps should be (s0 s1) and
;; (s3 s4) with 3 beacons shared for each pair, and X showing a beacon visible
;; only by the closest scanner. Could the algorithm wrongly deduce one or more
;; of wrong overlap pairings (s0 s3), (s0 s4), (s1 s3), (s1 s4)?   

;; The answer is no, but only because there are unshared beacons:

;;                  (large region having beacons/scanners 
;;          s1     / beyond visibility of all shown scanners)
;;          |     / 
;;     ...B......||...........B'.
;;     B....S...X||........B'...S --s3
;;     ....B.....||...X'.......B'
;;     S.........||........S.....
;;     |                   |
;;     s0                  s4
;;
;; Looking at wrong pairing (s0 s4), the B' relationships are considered equi- 
;; valent. However X' is closer to s0 than any B', yet is not part of s1's 
;; shared beacons and therefore s0's beacons (X' is certainly not listed on
;; s0's report). Therefore s0 and s4 can't be considered overlaps.

;; What about s0 and s3?, X' is not part of s3's report... What if there are
;; no excluded beacons or they are placed inconclusively?

;; Consider the same diagram, but with the X/X' placed beyond any B' from 
;; the s3/s0 viewpoints respectively:

;;                  (large region having beacons/scanners 
;;          s1     / beyond visibility of all shown scanners)
;;          |     / 
;;    ....B......||...........B'.....
;;    .B....S....||........B'...S.... --s3
;;    .....B.....||............B'....
;;    XS.........||........S........X'
;;     |                   |
;;     s0                  s4

;; In the above scenario, there is no way to prevent a wrong overlap 
;; deduction, since X and X' beacons are further away from the opposite
;; scanner than the B/B' beacons.

;; In the abscence of properly placed exclusive beacons contradicting the 
;; equivalences, we have two distinct islands of beacons and scanners mistaken
;; as one. Therefore we have to conclude that the presence or absence of 
;; contradicting evidence can always be used, or else the problem is not
;; well defined: in other words the actual input is such that the scenario
;; above can never occur. As a possible hint into the problem's construction,
;; it is likely that the threshold quantity was introduced as a way to prevent
;; non-deterministic outcomes; if that is the case than the number of 
;; equivalences may be sufficient, but we can't rely on it.

;;       Multiple Segments with Identical Deltas
;;
;; Some configurations can yield shared beacons forming segments which
;; can result in two distinct segments made of shared points having the 
;; same deltas, with the need to prevent one segment endpoint from being
;; mistaken for belonging to another segment:

;;        ...B..
;;        B....S'
;;        ....B.
;;        S.B...
;;        ......
;;        ...B..

;; In the above setup, segments (as seen from S on the left) ([0 2][3 -2])
;; and ([3 3][4 1]) have the same delta, and could have one or more of their
;; endpoints mistaken for one another, if we fail to consider their positional 
;; order - since no matter the respective positions of S and S', in their
;; 'correct' orientations both coordinate systems see the same segment
;; being the lower of the two. Therefore the segments have to be ordered so that
;; the algorithm sees shared segments from each side similarly, and the first
;; of multiple shared segments is chosen.

;;       Equivalent segments with some/all endpoints unshared 
;;
;; Consider the following setup:
;; 
;;  ......B'.
;;  .....B..S'
;;  S........

;; If the visibility distance is 5 or less then the upper beacon is unvisible
;; to S, and therefore the segment (B B') is not visible in S and no false   
;; equivalence can be detected this way. 

;; To summarize, the number of equivalent points together with the presence
;; of non-equivalent points throughout the scanner system ensure deterministic
;; overlap compuatability.

;;                     Overlap algorithm outline:
;;
;; Definitions: 
;;       System: a distinct relative coordinate system, i.e. with its absolute
;;               position unknon. Depending on context, can also/instead designate
;;               locations it applies to.
;;       Delta: the difference between respective coordinates of two points,
;;              expressed as coordinates, e.g. delta([1 2 3] [0 5 3]] = [-1 3 0]
;;       Segment Delta: the delta of respective endpoints of a segment.
;;       Segment Equivalence: when a pair of segments from two different systems,
;;                            have the same delta.
;;       Point Equivalence: when two points from two different systems and taken
;;                          from respective segments forming a segment equivalence,
;;                          are also the same point (shared beacon). 
;;       Translation Delta: the reverse of the delta between equivalent points, 
;;                          or the quantity to add to any point in the 
;;                          coordinate system of the second point to convert it
;;                          to coordinates in the system of the first point.
;;       Head:  the first endpoint of the first segment in a pair of segments.
;;
;; Input: 
;;      - s1 a sorted collection of points in its own system. 
;;      - s2, ditto
;;      - threshold, a required number of distinct point equivalences to be found
;;        in order to have an overlap.
;;
;; Output: If an overlap is found, a translation delta to be applicable to s2
;;         coordinates to obtain equivalent s1 coordinates; nil otherwise. 
  
;;                     Initialization:  
;;
;; Step 0.1: Generate all ordered segments (pairs of points) segs1 and segs2 
;;           from each of s1 and s2, s.t. segs1 contains all 2-combinations of 
;;           points from s1, and likewise for s2; each segment is ordered.
;;
;; Step 0.2: Sort segs1 and seg2 so that two consecutive segments compare like so: 
;;           [a1 b1 :as seg1] [a2 b2 :as seg2] => a1 <= a2, b1 <= b2 by order
;;           of priority, e.g. b1 > b2 => a1 <= a2. 
;;
;; Step 0.3: Create 'seg-pairs' from all 2-combinations from segs1 and segs2, by
;;           taking the first segment of segs1 and generating a pair from it and 
;;           each segment of segs2 in order, and so forth.
;;
;; Step 0.3: Create an empty set 'assigned?', containing points of s1 already 
;;           assigned a coordinate in s2's system.
;;
;; Step 0.4: Create a counter 'k' = 0, and an unassigned delta 'd' 
;;
;;                     Main:
;;
;; Step 1: If k = threshold,
;;         Then:
;; Step 1.1 Return the delta
;;         Else:
;; Step 1.2     If seg-pairs is empty
;;              Then:
;;              Return Nil
;;              Else continue to step 2 
;;
;; Step 2:    If the next unseen segment pair SP causes a segment equivalence, 
;;            Then:
;; Step 2.1:       If the head is Not in 'assigned?'
;;                 Then
;; Step 2.1-1:         If the delta d is unassigned, 
;;                     Then 
;; Step 2.1-1.1:           d = -(delta of SP)
;; Step 2.1-2:       k += 1  
;; Step 2.2:         Add the head to 'assigned?'
;; Step 3: Go back to step 1.


(defn delta
"Yields the difference vector or delta, from a to b, i.e. b - a."
 [[ax ay az :as a] [bx by bz :as b]]
  [(- bx  ax) (- by ay) (- bz az)])


(defn <-delta
  "p1 and p2 are equivalent points for the same beacon as seen from 
  different scanners.
  
  The same as (delta p2 p1). Yields the delta required to convert any 
  point in s2 coordinates to s1 coordinates, or the origin of p2's 
  coordinate system as p1 coordinates.
  "
  [[x1 y1 z1 :as p1] [x2 y2 z2 :as p2]]
  [(-> x2 - (+ x1))
   (-> y2 - (+ y1))
   (-> z2 - (+ z1))])

(defn delta-within? 
  ([threshold p1 p2]
   (->> (delta p1 p2) 
        (every? #(-> % abs (<= threshold))))))


(defn equivalent?
  "Yields true if respective ratios between coordinates of point a and those of
  point b are equal; false otherwise."
 [[[ax1 ay1 az1 :as a1] [ax2 ay2 az2 :as a2] :as sys-a]
  [[bx1 by1 bz1 :as b1] [bx2 by2 bz2 :as b2] :as sys-b]]
  (let [da (delta a1 a2)]
    (when (= da (delta b1 b2))
      da)))


(defn shift-by [[dx dy dz] [x y z]]
  [(+ x dx) (+ y dy) (+ z dz)])

(defn aligned-segments
  "Sorts right side segments of same delta segment pairs,
  and returns segment pairs, discarding the segment deltas"
  [delta+segs]
  (->> delta+segs
       ((juxt #(->> % (map second) sort)
              #(->> % (map last) sort)))
       (apply map vector)
       distinct))

(defn delta=-segments
  "Yields triples [delta pair1 pair2] from matching segments from two different
  systems, whenever the segments have the same delta between their respective
  endpoints. If opt map is provided, will check for a minimum of distinct 
  endpoints to be part of the returned value and if not, will return nil.
  Otherwise any number of tuples is returned.
 "
 [segs1 segs2]
   (for [pair1 segs1 
         pair2 segs2
         :let [delta (equivalent? pair1 pair2)]
         :when delta]
     [delta pair1 pair2]))

(defn equivalent-segments
  "segs1 and segs2 are sequential colls of segments in their respective coordinate
  systems. Yields all duples [seg1 seg2] of equivalent segments b/w segs1 and segs2.
  "
  [segs1 segs2]
  (->> (delta=-segments segs1 segs2)
       (group-by (fn [[d seg1 seg2]]
                   d))
       (sequence (comp
                  (map #(->> % second))
                  (mapcat aligned-segments)))))

(defn equivalence
  ([segs1 segs2 & [threshold]]
   (let [threshold (when-not (= :all threshold)
                     (or threshold *threshold*))]
     (loop [eqs (equivalent-segments segs1 segs2), k 0, 
            assigned {}, assigned-val? #{} assigned-segs []]
       (cond-let
        (or (<= threshold k)
            (empty? eqs))
        :>>
        [assigned assigned-segs]

        (and assigned-head? assigned-tail?)
        [eq (first eqs)
         head (ffirst eq), head' (-> eq second first)
         tail (-> eq first second), tail' (-> eq second second)
         assigned-head'? (assigned-val? head')
         assigned-tail'? (assigned-val? tail')
         assigned-head? (assigned head)
         assigned-tail? (assigned tail)]
        (recur (rest eqs) k assigned assigned-val? assigned-segs)
        
        (and assigned-tail? assigned-head'?)
        :>>     ;; not one-one
        nil 

        assigned-tail?
        :>>
        (recur (rest eqs) (+ k 1) 
               (-> assigned (assoc head head'))
               (-> assigned-val? (conj head'))
               (conj assigned-segs eq))

        (and assigned-head? assigned-tail'?)
        :>> ;; not one-one
        nil

        assigned-head?
        :>>
        (recur (rest eqs) (+ k 1)
               (-> assigned (assoc tail tail'))
               (-> assigned-val? (conj tail'))
               (conj assigned-segs eq))

        :else
        (recur (rest eqs) (+ k 2)
               (-> assigned (assoc head head') (assoc tail tail'))
               (-> assigned-val? (conj head') (conj tail'))
               (conj assigned-segs eq)))))))


(defn sufficient-equivalence?
"Yields a seq of pairs of equivalent segments from segs1 and segs2, 
  if their number is at or above a threshold (defaulting to *threshold*);
  nil otherwise.
"
[segs1 segs2 & [threshold]]
  (when-let [[eq] (equivalence segs1 segs2)]
    ;; (println :EQ eq)
    (when (<= (or threshold *threshold*) (count eq))
      eq)))

(defn consistent?
  "s1 and s2 are colls of points in two different coordinate systems, and
  m1 and m2 are sequentials of parallel equivalent points in s1 and s2's
  coordinate systems respectively; delta is the vector addition needed
  to convert s2 points into s1 coordinates.

  Returns true iff s2 has no point excluded from m2 which is visible
  from s1 and vice versa for s1 m1 and s2; nil otherwise."

  ([s1 s2 s2-origin m]
   (let [s1-shared (->> m (map first) set) 
         s1-only (-> s1 set (difference s1-shared))
         s2-shared (->> m (map second) set)
         s2-only (->> (-> s2 set (difference s2-shared))
                      (map (partial shift-by s2-origin))
                      set)]
     (and (->> s2-only (every? #(not (delta-within? *visibility* % [0 0 0]))))
          (->> s1-only (every? #(not (delta-within? *visibility* s2-origin %))))))))


(defn overlap? 
"If an overlap occurs b/w s1 and s2 in their current orientation's coordinates, 
 yields the translation delta, the coordinates of the origin of s2 relative to 
 the origina of s1; nil otherwise."
  ([s1 segs1 s2 segs2 & [threshold]]
   (cond-let 
    (< k threshold)
    [[eqmap] (equivalence segs1 segs2 threshold)
     k (count eqmap)
     threshold (or threshold *threshold*)]
    nil
    
    (and (<= threshold k)
         (consistent? s1 s2 tdelta eqmap))
    [tdelta (apply <-delta (first eqmap))]
    tdelta

    :else
    nil)))


;; Now on to finding a way to orchestrate overlap detection, in a way that 
;; mitigates the combinatorial nature of processing all possible rotations 
;; for each system.

;; 1), since we use the first scanner as reference, we don't need to
;; rotate it, and generate its segments only once: these will be reused for
;; testing all overlaps with scanner 0. 
;;
;; 2), all overlaps found with scanner 0 have their coordinates rotated  
;; suitably: if scanner S's region overlaps with s0 then its orientation is
;; fixed, and subsequent overlaps (other than with s0) will likewise be adapted
;; in a way that creates a unique custom rotation for each system relative to
;; its original configuration.
;;
;; 3) for the purpose of computing all absolute positions, not all overlaps
;; may need discovery, as long as each system has a known direct or indirect
;; overlap with scanner 0: that is because each overlap yields a translation
;; delta accumulated along the path.
;;
;; In light of this, after each connection the overlapping systems have their
;; configuration and therefore coordinates and segments thereof, fixed. 
;; We also want to resist merging two overlapping systems into one as long as
;; there are still un-detected overlaps; otherwise each merge creates a 
;; combinatorial explosion of segment generation. This means that although
;; the rotation is fixed and necessary, there is no need yet to translate the 
;; coordinates to the origin of the first sytem, as there may be more such
;; translations to perform upstream.
;;
;; The bulk of the work is generating transforms and segments thereof, and
;; testing for unsuccessful overlaps; if discovery is organized in a certain
;; way, a system having participated in two overlap detections can be cast
;; aside, or locked, without further testing. 
  
;; Let's first have a shell function which does the grunt work of generating
;; transforms and segments before testing for an overlap, and returning the
;; newly connected system's suitable configuration together with its overlapping
;; information (the translation delta of its rotated coordinates into those of
;; the previously connected system it is overlapping with):

(defnp transforms-from-cache [id locs {:keys [xfs u-xfs] :as cache}]
  (log 2 "transforms-from-cache: "
       (if (and id cache (xfs id)) 
         (str id " found")
         (str id " not found")))
  (if (and id cache)
    (or (xfs id)
        (u-xfs id (p ::gen-transforms 
                     (->> locs transform (map-indexed vector)))))
    (->> locs transform (map-indexed vector))))

(defnp segments-from-cache [id [idx tlocs] {:keys [segs u-segs] :as cache}]
  (log 2  "segments-from-cache:" [id idx]
       (if (and id cache (segs id idx))
          "found" "not found"))
  (if (and id cache)
    (or (segs id idx)
        (u-segs id idx (p ::gen-segments (u/segments tlocs))))
    (u/segments tlocs)))

(defn find-overlap 
"s1 and segs1 are coordinates (relative to [0 0 0], the origin as seen from
 the scanner of s1) and segments thereof, of a connected system, and s2
 is a collection of coordinates in some orientation, relative to s2's scanner.
 
 Finds the overlap if any, between s1 in its current orientation and some 
 orientation of s2, and nil otherwise. If found, the return value is a tuple 
 [s2t, s2t-segs, tdelta], where s2t is the coll. of s2 points after being
 rotated, s2t-segs the segments thereof, and tdelta the delta vector to add 
 to s2t coordinates to obtain s1 coordinates.

 If provided, `threshold` gives the minimum number of shared points required
 for having an overlap (defaults to `*threshold*, and `cache` allows reuse of
 generated transforms and segments in this and/or future calls (defaults
 to nil).

 If provided, memo must provide:
    id, the id of the scanner for s2
    xfs, a fn taking a scanner-id and returning all transforms for its points 
    u-xfs, an update fn taking a scanner-id and transforms for reuse 
           in future calls and returning the transforms fluently.
    segs, a fn taking a scanner id and transform idxs and returning segments
          for that transform
    u-segs, a fn taking a scanner id, transform id and segments for reuse 
           in future calls, and returning the segments fluently.  
    "
([s1 segs1 s2 & [threshold s2-id {:keys [xfs u-xfs segs u-segs] :as cache}]]
 (let [indexed-s2xfs (->> cache 
                          (transforms-from-cache s2-id s2))]
   (->> indexed-s2xfs
        (u/psome 
         (fn [[idx s2t :as i+t]]
           (let [s2t-segs (segments-from-cache s2-id i+t cache)]
             (when-let [<delta (overlap? s1 segs1
                                         s2t s2t-segs
                                         threshold)]
               (log 1 
                    "*** Exiting find-overlap with overlap"
                    (if s2-id 
                      (str "attaching " s2-id) "") "***\n")
               [s2t s2t-segs <delta]))))))))

(defn find-overlap-with-cache [s1 segs1 s2 s2-id cache]
  (find-overlap s1 segs1 s2 nil s2-id cache))

(defn new-cache []
  (let [cache (atom {:xforms {}
                     :segs {}})]
    {:xfs (fn [id]
            (-> @cache :xforms (get id)))
     :u-xfs (fn [id idx+xfs]
              (swap! cache assoc-in [:xforms id] idx+xfs)
              idx+xfs)
     :segs (fn [id idx]
             (-> @cache :segs (get-in [id idx])))
     :u-segs (fn [id idx segs]
               (swap! cache assoc-in [:segs id idx] segs)
               segs)}))

;; The individual overlaps form the edges of a connected graph between the
;; scanned regions (it is also a directed graph if we also consider the order
;; of discovery). 

;;                   Overall Algorithm Summary
;; Definitions:
;;
;;  - System: a collection of coordinates reflecting a scanner and its beacons,
;;            either its initial reported configuration or some rotation thereof,
;;            all coordinates relative to the scanner, i.e. [0 0 0].
;;
;;  - Referent: the system chosen to be the reference (having scanner 0).

;;  - Grounded: a system with at least one known overlap, except for the referent
;;              which is always grounded as a special case.
;;
;;  - Anchor: a grounded system against which not all ungrounded systems have 
;;            been tested for overlap with. The referent is the only anchor 
;;            initially. 
;;
;;  - Attachment: a system overlapping with an anchor, becoming grounded after
;;                the anchor
;;
;;  - Locked: a grounded system which is not an anchor: for example the referent
;;            anchor becomes locked once all its   

;; The idea is to, starting with scanner 0 grounded, repeatedly find overlaps
;; between all ungrounded systems and anchors, saving suitably rotated
;; systems and their conversion deltas along the way. Eventually we obtain
;; a directed graph. Every node (system) can be converted to its absolute 
;; positioning by following its edge path to the referent.
;;
;; 

;;                   Graph Algorithm Outline
;; Input: A collection of systems s0 to sN in order, each of which a tuple 
;;        [scanner id, system] 
;;
;; Output: a map of scanner ids to resolutions. Each resolution is
;;         a map of: 
;;        - locs, 3D coordinates of this system 
;;        - anchor, the scanner id of the anchor this system attaches to,
;;                  (the directed nature of the graph)    
;;        - delta, the translation used to convert to the anchor's coordinate
;;          system.
;;         
;; Initialization: - an empty map, Locked (storing the output), denoted L
;;                 - an empty set Grounded, denoted G.  
;;                 - an empty set Anchors, denoted A.
;;                 - a sequence of unprocessed input items to process, denoted U.

;; Step 0.1: Compose a triple [0 beacons nil nil] from the first input element 
;;           and add it to A.
;; Step 0.2: Add s1..sN to U.
;;
;; Step 1: For each element u of U,
;; Step 2:     For each element a of A,
;; Step 3:          Find an overlap a->u if any 
;; Step 4.1:                If an overlap exists 
;;                          Then:
;;     4.1-1.1:                   Add a tuple [id locs, a->g-delta, anchor-id] to G
;;     4.1-1.2:                   Remove u from U
;;     4.1-1.3:                   Go back to step 1
;;                          Else:
;;     4.1-2:                     Go back to step 2
;; Step 5:      Move all elements in A to L
;; Step 6:      Move all elements in G to A. 
;; Step 7:      Go back to step 1.
;; Wrap up:
;; Step 8.1:  Move all elements in A to L, 
;       8.2:  Move all elements in G to L, 
;;      8.3:  Return L

(defn ->node [locs delta anchor]
  {:locs locs :delta delta :anchor anchor})

(defn into-resolved [resolved anchors]
  (->> anchors 
       (into resolved 
             (map (fn [[id [locs _ d anchor-id]]]
                    [id (->node locs d anchor-id)])))))

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
          (if-let [[[t tsegs d] anchor-id]
                   (u/psome 
                    (fn [[id [locs segs]]]
                      (log 1 "Testing for overlap b/w" uid "and anchor" id)
                      (when-let [o (find-overlap-with-cache 
                                    locs segs ulocs uid cache)]
                        (log 1 "Attaching" uid "to anchor" id)
                        [o id]))
                    anchors)]
            [(conj gs [uid [t tsegs d anchor-id]]) not-gs]
            [gs (conj not-gs u)])))))

(defn ->graph 
  ([reports]
   (->graph reports nil))
  ([reports ||size]
   (->graph reports :use-cache ||size))
  ([sId+locs use-cache? ||size]
   (let [[s0-id s0-locs] (first sId+locs)
         cache (when use-cache? (new-cache))]
     (loop [todo (rest sId+locs) 
            prv-todo nil
            anchors [[s0-id [s0-locs (u/segments s0-locs)]]]
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
           (log 1 "Resolved" (+ (count resolved)
                                (count anchors)
                                (count gs)) "regions," 
                (count ungs) "to go\n" (apply str (repeat ctr \.)))
           (recur ungs todo gs (into-resolved resolved anchors) (inc ctr))))))))

;;                   Assemble Algorithm Outline
;; Inputs: 
;;   - A map of scanner id -> resolution from the output of the Graph Algorithm
;;
;; Output: - A collection of points of absolute positions
;;
;; Step 1: For each node [points, delta,  
;; Step 2:   Compute its cumulative delta CD, with function total-delta(R, Resolutions) 
;; Step 3:   Convert R's report (by looking it up using id) beacons by adding
;;           CD to each point 
;; Step 4:   Add the converted points to set A.
;; Step 5:   Repeat step 1 with the next R

;;                 Total-Delta Algorithm Outline
;; Inputs: 
;;    - A resolution R tuple with a partial delta D
;;    - All resolutions Rs 
;; Step 1: If D is zero, return 0 
;; Step 2: Look up in Rs the resolution R' with id = r's anchor-id
;; Step 3: Return R's delta added to the result of invoking the 
;;         function recursively with R'


(defnp cumulative-delta [id graph]
  (loop [acc [0 0 0] {:keys [anchor delta]} (graph id)]
    (if (nil? delta)
      acc 
      (recur (shift-by delta acc)
             (graph anchor)))))

(defn assemble [graph]
  (->> graph
       (reduce (fn [acc [id {:keys [locs]}]]
                 (let [d (cumulative-delta id graph)]
                   (->> locs
                        (into acc 
                              (map #(shift-by d %))))))
               #{})))

(defn answer1 [input]
  (-> input (->graph nil) assemble count))


;; (= (-> sample ->graph assemble set) (set sample-beacons-0-position))
;; true
;; (answer1 sample)
;;=> 79

;; Do not uncomment this, but rather run it at the repl - almost 5 minutes! 
;; (time (answer1 input))
;; "Elapsed time: 241324.069689 msecs"
;; 378

;;;;   PART 2 ;;;;;;;


(defn scanner-positions [g]
  (->> g keys (mapv #(cumulative-delta % g))))

(defn answer2 [input-or-graph & [||size]]
  (let [g (if (map? input-or-graph) 
            input-or-graph
            (-> input (->graph ||size)))]
    (->> g 
         scanner-positions
         u/segments
         (map (partial apply delta))
         (map #(mapv abs %))
         (map (partial apply +))
         (apply max))))

;; (= sample-max-scanner-manhattan-distance (answer2 sample))
;; true

;; (answer2 input)
;; 13148
