(ns advent-of-code.-2021.day18.day18-test
  (:refer-clojure :exclude [reduce])
  (:require [advent-of-code.-2021.day18.day18
             :refer :all]
            [advent-of-code.-2021.day18.input :as i
             :refer [sample1 
                     sample2 sample2-sol sample2-sol-intermediate-results
                     sample3-step1
                     sample3-step2
                     sample3-step3
                     sample3-step4
                     sample3-step5
                     tiny-sample1 tiny-sample1-sol
                     tiny-sample2 tiny-sample2-sol
                     tiny-sample3 tiny-sample3-sol
                     magnitude-samples+sols
                     homework-mag
                     homework-sample
                     homework-sol
                     input
                     explode-samples explode-sols]]
            [clojure.test :refer [deftest testing are is]]
            [clojure.zip :as z]))


(deftest nav-test
  (testing "navigating to an exploding snumber"
    (are [snum exp]
        (let [[act ] (-> snum z/vector-zip nested-loc)]
          (is (= exp act)))

      [[[[[9 8] 1] 2] 3] 4]
      [9 8]

      [[[1 [[2 3]]] 4] [5 6]]
      [2 3]
 
      [[[1 [2 3]] 4] [5 6]]
      nil

      [[[1 23] 4] [5 6]]
      nil

      [[[1 2] [3 [4 [5 6]]]] 7]
      [5 6]
      
      [7 [6 [5 [4 [3 2]]]]]
      [3 2]
      
      [[6 [5 [4 [3 2]]]] 1]
      [3 2]
      
      [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]]
      [7 3]
      
      [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]]
      [3 2]

      [[3 [2 [8 0]]] [9 [10 [11 [12 13]]] [5 [4 [3 2]]]]]
      [12 13]
      
      [[1 [2 [[3 4] 5]]] 7]
      [3 4]
      
      [[[[1 [2 3]]]], [4 [5 [[6 7]]]]]
      [2 3])))


(deftest explode-test
  (testing "explode examples from instructions"
    (are [idx]
        (is (= (nth explode-sols idx)
               (-> explode-samples (nth idx)
                   z/vector-zip
                   do-explode 
                   z/root)))
      0 1 2 3 4))
  (testing "individual explode scenarios"
    (are [snum exp]
        (is (= exp (-> snum z/vector-zip do-explode z/root)))
      
      [[[[[4 3] 4] 4] [7 [[8 4] 9]]] [1 1]]
      [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]]
      
      [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]]
      [[[[0 7] 4] [15 [0 13]]] [1 1]]
      
      [[[[0 7] 4] [15 [0 13]]] [1 1]]
      [[[[0 7] 4] [15 [0 13]]] [1 1]]


      [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]
      [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]
      

      [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]]
      [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]
      

      [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]
      [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]
)))

(deftest split-test
  (are [snum exp]
      (is (= exp
             (-> snum z/vector-zip do-split z/root)))
    [[10 1] 2]
    [[[5 5] 1] 2]

    [[1 2] [3 [17 [4 5]]]]
    [[1 2] [3 [[8 9] [4 5]]]]
    
    [1 2]
    [1 2]


    [[[[0 7] 4] [15 [0 13]]] [1 1]]
    [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]
    

    [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]
    [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]]

))

(deftest reduce-test
  (are [sample sol]
      (is (= sol (reduce sample)))
    i/sample3-step1 i/sample3-step5))


(deftest sum-test
  (are [sample sol]
      (is (= sol (sum sample)))
    tiny-sample1 tiny-sample1-sol
    tiny-sample2 tiny-sample2-sol
    tiny-sample3 tiny-sample3-sol
    sample2 i/sample2-sol
    homework-sample homework-sol))


(deftest magnitude-test
  (testing "magnitude example in instructions"
    (let [mags (->> magnitude-samples+sols (map first))
          sols (->> magnitude-samples+sols (map second))]
      (are [idx]
          (= (nth sols idx) (magnitude (nth mags idx)))
        0 1 2 3 4 5))
    (is (= homework-mag (magnitude homework-sol)))))
