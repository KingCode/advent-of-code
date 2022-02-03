(ns advent-of-code.-2021.day19.day19-better-test
  (:require [advent-of-code.-2021.day19.day19-better :refer :all]
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


(def feature-1
  [[[0 2 0] [2 0 0] [4 1 0] [3 3 0] [3 -2 0]]
   [[-1 -1 0] [-5 0 0] [-3 -2 0] [-2 1 0] [-2 -4 0]]])


(deftest delta-test
  (are [ax ay az bx by bz ox oy oz]
      (= [ox oy oz] 
         (delta [ax ay az] [bx by bz])
         #_(delta [bx by bz] [ax ay az]))
    0 2 0 -5 0 0, 5 2 0))

(deftest overlap?-test
  (are [s1 s2 threshold exp]
      (is (= exp (overlap? s1 s2 threshold)))
    [[0 2 0] [4 1 0] [3 3 0]] 
    [[-1 -1 0] [-5 0 0] [-2 1 0]]
    3
    [5 2 0]

    [#_[0 2 0] [4 1 0] [3 3 0]] 
    [[-1 -1 0] #_[-5 0 0] [-2 1 0]]
    3
    nil))
