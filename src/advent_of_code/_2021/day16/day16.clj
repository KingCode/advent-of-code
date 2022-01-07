(ns advent-of-code.-2021.day16.day16
  (:require [advent-of-code.-2021.day16.input
             :refer [sample-type4 
                     sample-type4-literal-bin
                     sample-type4-value
                     sample-type-length0
                     sample-type-length0-value
                     sample-type-length1
                     sample-type-length1-value
                     sample1 sample1-value sample1-vid-sum
                     sample2 sample2-value sample2-vid-sum
                     sample3 sample3-value sample3-vid-sum
                     sample4 sample4-value sample4-vid-sum
                     
                     part2-sample1 part2-sample1-value
                     part2-sample2 part2-sample2-value
                     part2-sample3 part2-sample3-value
                     part2-sample4 part2-sample4-value
                     part2-sample5 part2-sample5-value
                     part2-sample6 part2-sample6-value
                     part2-sample7 part2-sample7-value
                     part2-sample8 part2-sample8-value

                     input]]
            [advent-of-code.-2021.day16.util 
             :refer [hex->bin 
                     hex->bits
                     bits->dec 
                     bits->ints 
                     binv->dec 
                     int0
                     split-str-at]]
            [advent-of-code.util.cond-let :refer [cond-let]]))

;; Most of the work in this challenge was actually understanding how parsing
;; works by going through the examples manually. `day16.util` functions
;; were initially created to help verify hand tracing the examples.

;; As in many translating problems such as writing interpreters, 
;; a uniform parse operation is applied to different types
;; polymorphically. Even though part 1 requires only two types (literals and 
;; ops), I expected (wrongly) more of them to appear in part 2, hence my 
;; decision to use a multi method in part 1 (see `parse-body` below).
;; However even with only two types, in this case a multi-method feels right. 

;; Some vocabulary items first, just to avoid confusion:
;;    - Bits, rather than bytes, are the primitive material in and out of 
;;      parsing funtions, because of their arbitrary granularity not aligned 
;;      with byte size multiples. 
;;    - A packet is a rendered triple, the result of decoding raw bytes
;;    - A value is the payload, or last element of a packet

;; We model a packet as follows:
;;
;;   [version-id type-id value]
;;
;; where the value is either a literal or a sequence of packets.

;; A sequence of bits behaves as a stream and so parsing it requires 
;; 'moving the tape' and consuming some of it to produce packets. 
;; To emulate this, a parse result consists of a packet and the 
;; unconsumed remaining bits which are fed back in recursively. 

;; As an aside, the `case` and `cond` macro are especially well suited 
;; without a default clause to make branching bugs more obvious. 

(defn parse-n-field [n bits]
  (let [[fld bits] (split-str-at n bits)]
    [(bits->dec fld) bits]))

(defn parse-subcount [bits]
  (let [[numbits rbits] (split-str-at 11 bits)]
    [[:subcount (bits->dec numbits)] rbits]))

(defn parse-subsize [bits]
  (let [[lenbits rbits] (split-str-at 15 bits)]
    [[:subsize (bits->dec lenbits)], rbits]))

(defn parse-length [bits]
  (let [[mode rbits] (split-str-at 1 bits)]
    (case mode
      "0" (parse-subsize rbits)
      (parse-subcount rbits))))

(defn parse-header [bits]
  (let [[vid nov-bits] (parse-n-field 3 bits)
        [tid rbits] (parse-n-field 3 nov-bits)]
    [[vid tid] rbits]))

(defn packet-type [tid]
  (case tid
    4 :literal
    (0 1 2 3 5 6 7)
    :op))

(declare parse)

(defmulti parse-body (fn [tid _] (packet-type tid)))

(defn bytefrags->dec [bytefrags ]
  (->> bytefrags
       (apply concat) (apply str)
       bits->dec))

(defmethod parse-body :literal [_ bits]
  (->> (range) 
       (reduce (fn [[fragments todo] _]
                 (let [[mark frag] (->> todo (take 5) 
                                        (split-str-at 1))
                       todo' (drop 5 todo)]
                   (case mark
                     "0"
                     (reduced [(bytefrags->dec 
                                 (conj fragments frag))
                               todo'])
                     "1"
                     [(conj fragments frag) todo'])))
               [[] bits])))

(defn parse-n-packets [n bits]
  (->> (range n 0 -1)
       (reduce (fn [[packets todo :as acc] _]
                   (let [[newpack todo'] (parse todo)]
                     [(conj packets newpack) todo']))
               [[] bits])))

(defn parse-size-packets [size bits]
      (->> (range)
           (reduce (fn [[packets todo] _]
                     (if (empty? todo)
                       (reduced [packets (drop size bits)])
                       (let [[newpack todo'] (parse todo)]
                         [(conj packets newpack) todo'])))
                   [[] (->> bits 
                            (take size))])))


(defmethod parse-body :op [_ bits]
  (let [[[kind n] todo] (parse-length bits)]
    (case kind
      :subcount 
      (parse-n-packets n todo)
      :subsize
      (parse-size-packets n todo))))

(defn parse [bits]
  (let [[[vid tid] bits] (parse-header bits)
        [body rbits] (parse-body tid bits)]
    [[vid tid body] rbits]))

(defn packet [hex]
  (-> hex hex->bits parse first))

(= sample-type4-value (-> sample-type4 packet))
;;=> true
(= sample-type-length0-value (packet sample-type-length0))
;;=> true
(= sample-type-length1-value (packet sample-type-length1))
;;=> true
(= sample1-value (-> sample1 packet))
;;=> true
(= sample2-value (packet sample2))
;;=> true
(= sample3-value (packet sample3))
;;=> true
(= sample4-value (packet sample4))
;;=> true

(defn extract-fields
  "Extracts atomic fields in the nth position of a decoded packet
  and likewise for each nested sub packet."
 [n pvalue]
  (cond-let 
   (> (inc n) (count pvalue))
   :>>
   (throw (ex-info "Packet tuple illegal index" {:n n :pvalue pvalue}))
   
   (coll? value)
   [tgt (nth pvalue n)
    value (last pvalue)]
   (->> value (map #(extract-fields n %)) flatten (cons tgt))
   
   :else
   [tgt]))

(defn extract-version [packet]
  (extract-fields 0 packet))

(defn answer1 [input]
  (->> input packet extract-version
       (apply +)))

(= sample1-vid-sum (answer1 sample1))
;;=> true
(= sample2-vid-sum (answer1 sample2))
;;=> true
(= sample3-vid-sum (answer1 sample3))
;;=> true
(= sample4-vid-sum (answer1 sample4))
;;=> true

(answer1 input)
;; => 993


;; PART 2

;; The new requirements simply added evaluation to previously decoded packets 
;; and here an `eval-packet` multi method is well suited to deal with the 
;; multiple operations.

(def value-types {4 :literal
                  0 :sum
                  1 :product
                  2 :min
                  3 :max
                  5 :gt
                  6 :lt
                  7 :=})

(defmulti eval-packet #(-> % second value-types))

(defmethod eval-packet :literal [pkt]
  (last pkt))

(defn eval-args-and-apply [op pkt]
  (->> pkt last 
       (map eval-packet)
       (apply op)))

(defmethod eval-packet :sum [pkt]
  (->> pkt (eval-args-and-apply +)))

(defmethod eval-packet :product [pkt]
  (->> pkt (eval-args-and-apply *)))

(defmethod eval-packet :min [pkt]
  (->> pkt (eval-args-and-apply min)))

(defmethod eval-packet :max [pkt]
  (->> pkt (eval-args-and-apply max)))

(defn eval-args-and-apply-pred [pred pkt]
  (if (->> pkt 
           (eval-args-and-apply pred))
    1
    0))

(defmethod eval-packet :gt [pkt]
  (->> pkt (eval-args-and-apply-pred >)))

(defmethod eval-packet :lt [pkt]
  (->> pkt (eval-args-and-apply-pred <)))

(defmethod eval-packet := [pkt]
  (->> pkt (eval-args-and-apply-pred =)))

(defn answer2 [hex]
  (-> hex packet eval-packet))

(= part2-sample1-value (answer2 part2-sample1))
;;=> true
(= part2-sample2-value (answer2 part2-sample2))
;;=> true
(= part2-sample3-value (answer2 part2-sample3))
;;=> true
(= part2-sample4-value (answer2 part2-sample4))
;;=> true
(= part2-sample5-value (answer2 part2-sample5))
;;=> true
(= part2-sample6-value (answer2 part2-sample6))
;;=> true
(= part2-sample7-value (answer2 part2-sample7))
;;=> true
(= part2-sample8-value (answer2 part2-sample8))
;;=> true

(answer2 input)
;;=> 144595909277
