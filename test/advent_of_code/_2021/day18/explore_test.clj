(ns advent-of-code.-2021.day18.explore-test
  (:require [advent-of-code.-2021.day18.explore :refer :all]
            [clojure.zip :refer [vector-zip]]
            [clojure.test :refer [deftest are is testing]]))


(deftest nav-test
  (testing "navigating to an exploding snumber"
    (are [snum exp-idx exp]
        (let [[_ act-idx] (-> snum vector-zip explode-kpath)]
          (is (= exp-idx act-idx))
          (is (= exp (and act-idx (get-in snum act-idx)))))

      [[[[[9 8] 1] 2] 3] 4]
      [0 0 0 0] [9 8]

      [[[1 [[2 3]]] 4] [5 6]]
      [0 0 1 0] [2 3]
 
      [[[1 [2 3]] 4] [5 6]]
      nil nil

      [[[1 23] 4] [5 6]]
      nil nil

      [[[1 2] [3 [4 [5 6]]]] 7]
      [0 1 1 1] [5 6]
      
      [7 [6 [5 [4 [3 2]]]]]
      [1 1 1 1] [3 2]
      
      [[6 [5 [4 [3 2]]]] 1]
      [0 1 1 1]  [3 2]
      
      [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]]
      [0 1 1 1] [7 3]
      
      [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]]
      [1 1 1 1] [3 2]

      [[3 [2 [8 0]]] [9 [10 [11 [12 13]]] [5 [4 [3 2]]]]]
      [1 1 1 1] [12 13]
      
      [[1 [2 [[3 4] 5]]] 7]
      [0 1 1 0] [3 4]
      
      [[[[1 [2 3]]]], [4 [5 [[6 7]]]]]
      [0 0 0 1] [2 3])))
