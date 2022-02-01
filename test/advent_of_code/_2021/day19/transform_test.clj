(ns advent-of-code.-2021.day19.transform-test
  (:require [advent-of-code.-2021.day19.transform :refer :all]
            [clojure.test :refer [deftest testing are is]]
            [clojure.set :refer [intersection difference]]))

(def feature-1
  [[[0 2 0] [2 0 0] [4 1 0] [3 3 0] [3 -2 0]]
   [[-1 -1 0] [-5 0 0] [-3 -2 0] [-2 1 0] [-2 -4 0]]])

(def feature-points [[1 1 1] [1 2 0]])

(deftest transform-coverage-test
  (let [xts (transform-1 [1 2 3])]
    (is (->> xts distinct count (= 24)))))

(deftest reverse-transform-test
  (are [n]
      (is (= (first feature-1) 
             (->> (first feature-1) 
                  (transform-n n)
                  (reverse-transform n))))
    0 1 2 3 4 5 6 10 11 12 13 14 15 16 17 18 19 20 21 22 23))


(deftest transform-accuracy-test
  (testing "single transforms on a chosen point"
    (let [p [1 2 3]]
      (is (= [1 2 3] (x-pos-0-point p)))
      (is (= [1 -3 2] (x-pos-1-point p)))
      (is (= [1 -2 -3] (x-pos-2-point p)))
      (is (= [1 3 -2] (x-pos-3-point p)))
      
      (is (= [-1 2 -3] (x-neg-0-point p)))
      (is (= [-1 3 2] (x-neg-1-point p)))
      (is (= [-1 -2 3] (x-neg-2-point p)))
      (is (= [-1 -3 -2] (x-neg-3-point p)))

      (is (= [2 -1 3] (y-pos-0-point p)))
      (is (= [2 -3 -1] (y-pos-1-point p)))
      (is (= [2 1 -3] (y-pos-2-point p)))
      (is (= [2 3 1] (y-pos-3-point p)))
      
      (is (= [-2 -1 -3] (y-neg-0-point p)))
      (is (= [-2 3 -1] (y-neg-1-point p)))
      (is (= [-2 1 3] (y-neg-2-point p)))
      (is (= [-2 -3 1] (y-neg-3-point p)))

      (is (= [3 2 -1] (z-pos-0-point p)))
      (is (= [3 1 2] (z-pos-1-point p)))
      (is (= [3 -2 1] (z-pos-2-point p)))
      (is (= [3 -1 -2] (z-pos-3-point p)))
      
      (is (= [-3 2 1] (z-neg-0-point p)))
      (is (= [-3 -1 2] (z-neg-1-point p)))
      (is (= [-3 -2 -1] (z-neg-2-point p)))
      (is (= [-3 1 -2] (z-neg-3-point p))))))
