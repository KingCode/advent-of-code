(ns advent-of-code.util.stream
"Provides streaming capabilities to sequences"
  (:refer-clojure :exclude [next]))

(defprotocol Streaming
  (nexte [_]))

(defprotocol Nav 
  (at [_] 
    "Yields the element at the cursor position")
  (pos [_] 
    "Yields the (zero-based) position of the cursor")
  (back [_] [_ n] 
    "Moves the cursor back by n (default 1) positions and yields its element.")
  (forward [_] [_ n]
    "Moves the cursor forward by n (default 1) positions and yields its element.")
  (seek [_ n]
    "Moves the cursor to the nth position (zero-based) and yields its element.")
  (rewind [_]
    "Moves the cursor back to the start position and yields its element.")
  (end [_]
    "Moves the cursor to the last position and yields its element.")
  (middle [_]
    "Moves the cursor to the (quot size 2) position and yields its element.")
  (marks [_]
    "Yields all current marks.")
  (mark [_ mark]
    "Adds a mark to the current cursor position.")
  (delmark [_ mark]
    "Removes a mark")
  (to [_ mark]
    "Moves the cursor to the marked position and yields its element.")
  (p [_] 
    "Moves to the previously accessed position in history and yields its element.")
  (n [_] 
    "Moves back to the next accessed position in history and yields its element.")
  (history [_]
    "Yields a history of all accesses' positions, listing marks as labels")
  (clear-history [_]
    "Clears the history")
  (clear-marks [_]
    "Clears the mars"))


(defn stream 
  "Yields a stream of xs"
  [xs]
  (let [state (atom {:stream xs})]
    (reify Streaming
      (nexte [_] (let [head (first (-> @state :stream))]
                          (swap! state update :stream rest)
                          head)))))

(defn chop [vcoll n]
  (subvec vcoll 0 n))

(defn navigator 
  "Yields a navigator for xs implemented using 'nth.
   Using other methods than next/prev during history 
  navigation removes historic items positioned after the 
  latest prev/next. 
  Invoking 'end, or 'middle methods may cause a hang-up 
  if the underlying collection is infinite.
  "
  [xs]
  (let [state (atom {:cursor 0
                     :src xs
                     :history  []
                     :marks {}})

        ;; check (fn [n]
                ;; (try (-> @state :src (nth n))
                     ;; (catch IndexOutOfBoundsException e
                       ;; (println "Error, bad index: " n))))
        len (fn []
              (-> @state :src count))

        value-at-and-set-pos 
        (fn [n]
          (try 
            (let [v (-> @state :src (nth n))]
              (swap! state (fn [m] 
                             (-> m  
                                 (assoc :cursor n)
                                 (update :history conj n))))
              v)
            (catch IndexOutOfBoundsException e
              (println "Error, bad index: " n))))
        absol 
        (fn [rel-n]
          (-> @state :cursor (+ rel-n)))]

    (reify Nav
      (at [_] (->> @state :cursor (nth (-> @state :src))))
      (pos [_] (-> @state :cursor))
      (back [_] (value-at-and-set-pos (absol -1)))
      (back [_ n] (value-at-and-set-pos (absol (- n))))
      (forward [_] (value-at-and-set-pos (absol 1)))
      (forward [_ n] (value-at-and-set-pos (absol n)))
      (seek [_ n] (value-at-and-set-pos n))
      (rewind [_] (value-at-and-set-pos 0))
      (end [_] (value-at-and-set-pos (dec (len))))
      (middle [this] (value-at-and-set-pos (quot (len) 2)))
      (marks [_] (-> @state :marks))
      (mark [this label] (do (-> state (swap! update :marks 
                                              assoc label (pos this)))
                             [:added label (at this)]))
      (delmark [_ label] (do (-> state (swap! update :marks
                                              dissoc label))
                             [:removed label]))

      (to [_ mark] (let [mpos  (-> @state :marks (get mark))]
                     (if (number? mpos)
                       (value-at-and-set-pos mpos)
                       (println "Mark" mark "not found."))))

      ;; TODO: implement (n [_]), (p [_]) and history update features

      (history [_] (-> @state :history))
      (clear-history [_] (-> state (swap! assoc :history []))))))

