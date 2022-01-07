(ns advent-of-code.util.continuation)

(defn continue [f]
  (fn [x]
    (f x)))

;; pattern, first manual rendition:
;; (def f1 (fn [x]
          ;; (cons 1 x)))

;; (def f2 (fn [x]
          ;; (f1 (cons 2 x))))

;; (def f3 (f2 nil))
;; f3
;; => (1 2) 

;; using continue:
;; (def a (continue (partial cons 1)))
;; (def b (continue #(a (cons 2 %))))
;; (b nil)
;; => (1 2) 

