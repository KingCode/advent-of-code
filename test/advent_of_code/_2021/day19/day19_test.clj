(ns advent-of-code.-2021.day19.day19-test
  (:require [advent-of-code.-2021.day19.day19 :refer :all]
            [advent-of-code.-2021.day19.input 
             :refer [sample 
                     sample-s0s1-0-overlap
                     sample-s0s1-1-overlap
                     sample-s0s1-overlap
                     sample-s1-0-position
                     sample-s1s4-0-overlap 
                     sample-s2-0-position
                     sample-s3-0-position
                     sample-s4-0-position
                     sample-beacons-0-position]]
            [advent-of-code.-2021.day19.transform :as t]
            [advent-of-code.-2021.day19.util :as u] 
            [clojure.test :refer [deftest are is testing]]
            [clojure.set :refer [subset? intersection difference union]]))


(deftest delta-test
  (are [ax ay az bx by bz ox oy oz]
      (= [ox oy oz] 
         (<-delta [ax ay az] [bx by bz])
         (delta [bx by bz] [ax ay az]))
    0 2 0 -5 0 0, 5 2 0))


(defn one-one? [m]
  (= (count m)
     (->> m (map second) distinct count)))

(defn revert0-point [n xfs loc]
  (->> xfs (take n) reverse
       (reduce (fn [loc f]
                 (f loc))
               loc)))

(def feature-1
  [[[0 2 0] [2 0 0] [4 1 0] [3 3 0] [3 -2 0]]
   [[-1 -1 0] [-5 0 0] [-3 -2 0] [-2 1 0] [-2 -4 0]]])

(defn eq [s0 s1]
  (let [s0-segs (u/segments s0)
        s1ts (t/transform s1)]
    (->> s1ts
         (map-indexed vector)
         (map (fn [[i pts]]
                [i, (delta=-segments s0-segs (u/segments pts))]))
         (filter #(-> % second seq)))))

(defn test-equivalence 
  ([segs0 s1xfs n] (sufficient-equivalence? segs0 (u/segments (nth s1xfs n)))))

(defn eq+eqs1+<delta+s1t [s0 s1]
  (let [s0-segs (u/segments s0) 
        s1ts (t/transform s1)
        [eq transform-n] 
        (->> s1ts 
             (map-indexed vector)
             (some (fn [[i pts]]
                     (let [segs (u/segments pts)]
                       (when-let [eq (sufficient-equivalence? s0-segs segs)]
                         [eq i])))))
        converted-eq 
        (->> eq (into {}
                      (map (fn [[kloc vloc]]
                             [kloc 
                              (t/reverse-transform-1 transform-n vloc)]))))
        <delta (when (seq eq) (apply <-delta (first eq)))]
    (when (seq eq)
      [eq converted-eq <delta (nth s1ts transform-n)])))


(deftest equivalence-test
  (testing "single segment equivalence"
    (are [ax1 ay1 az1 ax2 ay2 az2
          bx1 by1 bz1 bx2 by2 bz2] 
        (let [seg1 [[ax1 ay1 az1] [ax2 ay2 az2]]
              seg2 [[bx1 by1 bz1] [bx2 by2 bz2]]]
          
          (is (equivalent? seg1 seg2)))
      0 2 0, 4 1 0
      -5 0 0,-1 -1 0))
  (testing "multiple same-oriented segments"
    (let [s0 [[0 2 0] [2 0 0] [3 -2 0] [4 1 0][3 3 0]]
          s1 [[-1 -1 0] [-5 0 0] [-2 -4 0] [-3 -2 0] [-2 1 0]]
          segs0 (u/segments s0)
          segs1 (u/segments s1)
          [m] (equivalence segs0 segs1)]
      (is (= {[0 2 0] [-5 0 0]
              [2 0 0] [-3 -2 0]
              [3 -2 0] [-2 -4 0]
              [4 1 0] [-1 -1 0]
              [3 3 0] [-2 1 0]}
             m))))

  (testing "that equivalence is always one-to-one"
    (let [s0 [[1 1 0] [7 3 0] [0 2 0] [2 0 0] [3 -2 0] [4 1 0][3 3 0]]
          s1 [[2 1 0] [-1 -1 0] [-4 -1 0][-5 0 0] [-2 -4 0] [-3 -2 0] [-2 1 0]]
          segs0 (u/segments s0)
          segs1 (u/segments s1)
          [m] (equivalence segs0 segs1)]
      (is (= {[0 2 0] [-5 0 0]
              [2 0 0] [-3 -2 0]
              [3 -2 0] [-2 -4 0]
              [4 1 0] [-1 -1 0]
              [3 3 0] [-2 1 0]
              [1 1 0] [-4 -1 0]
              [7 3 0] [2 1 0]}
             m))
      (is (one-one? m))))

  (testing "sample equivalences"
    (let [s0 (-> sample first last)
          s1 (-> sample second last)
          [eq-s0s1-0, eq-s0s1-0->1, <delta-0-1, s1t :as res01] 
          (eq+eqs1+<delta+s1t s0 s1)
          s1-000  <delta-0-1
          s1-0 (->> s1t (mapv (partial shift-by <delta-0-1)))]
      (is (when res01 
            (is (= sample-s0s1-overlap eq-s0s1-0->1))
            (is (= sample-s1-0-position s1-000))))
      
      (let [[eq-s1s4-0 _ <delta-0-4 :as res14]
            (eq+eqs1+<delta+s1t s1-0 (-> sample (nth 4) last))
            act-overlap (->> eq-s1s4-0 (map first) sort)
            s4-000 <delta-0-4]
        (is (when res14
              (is (= (sort sample-s1s4-0-overlap) act-overlap))
              (is (= sample-s4-0-position s4-000))))))))


(deftest overlap?-test
  (are [s1 s2 threshold exp]
      (is (= exp (overlap? s1 (u/segments s1) 
                           s2 (u/segments s2) threshold)))
    [[0 2 0] [4 1 0] [3 3 0]] 
    [[-1 -1 0] [-5 0 0] [-2 1 0]]
    3
    [5 2 0]

    [#_[0 2 0] [4 1 0] [3 3 0]] 
    [[-1 -1 0] #_[-5 0 0] [-2 1 0]]
    3
    nil))

(defonce sample-graph-feature (->graph sample))

(deftest ->graph-test
  (is (= sample-s4-0-position
         (cumulative-delta 4 sample-graph-feature)))
  (is (= sample-s2-0-position
         (cumulative-delta 2 sample-graph-feature)))
  (is (= sample-s3-0-position
         (cumulative-delta 3 sample-graph-feature))))

(deftest assemble-test
  (is (= (set sample-beacons-0-position)
         (assemble sample-graph-feature))))
