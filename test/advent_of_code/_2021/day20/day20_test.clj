(ns advent-of-code.-2021.day20.day20-test
  (:require [advent-of-code.-2021.day20.day20 :refer :all]
            [advent-of-code.-2021.day20.input :as i]       
            [clojure.test :refer [deftest are is testing]]))


(deftest add-surrounding-indexes-test
  (are [rsiz csiz]
      (let [rcs (for [i (range rsiz)
                      j (range csiz)] 
                  [i j])
            [rr] (expand-boundaries rsiz)
            [cr] (expand-boundaries csiz)
            exp (for [i rr j cr] [i j])
            act (-> (apply sorted-set rcs)
                    (add-surrounding-indexes rr cr rsiz csiz)
                    seq)]
        (is (= exp act)))
    2 2
    5 5
    30 30
    7 8))

(deftest add-pos-expansion-indexes-test
  (are [rsiz csiz]
      (let [rcs (for [i (range rsiz) 
                      j (range csiz)] [i j])
            rsiz' (+ 2 rsiz)
            csiz' (+ 2 csiz)
            exp (for [i (range rsiz')
                      j (range csiz')] [i j])
            act (-> (apply sorted-set rcs)
                    (add-pos-expansion-indexes rsiz csiz rsiz' csiz')
                    seq)]
        (is (= exp act)))
    2 2
    3 4
    11 11
    35 40))
