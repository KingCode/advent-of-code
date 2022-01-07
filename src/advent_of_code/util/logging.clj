(ns advent-of-code.util.logging
  (:require [clojure.core.async :as ca
             :refer [go <!! <! >! chan close!]]))


(defn log [c & msg]
  (go (>! c msg))
  nil)

(defn start-logger [buf-or-n]
  (let [c (chan buf-or-n)]
    (future
      (loop [] 
        (when-let [msg (<!! c)]
          (apply println msg)
          (recur))))
    c))


(defn shutdown [c]
  (close! c))
