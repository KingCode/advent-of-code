(ns advent-of-code.util.repl
  ;; (:import [jline.console ConsoleReader])
  ;; (:import jline.Terminal)
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]))

(defn pager
  "Yields a paging fn which pauses after pretty-printing `pagesiz` items
  one per line, and will pause through subsequent items after any key press, 
  unless a value evaluating to `end` is entered by the user (defaults
  to :q)"
  [xs pagesiz end & {:keys [prompt? space?] :as opts}]
  (let [pages (atom xs)]
    (fn []
      (doseq [x (let [page (take pagesiz @pages)]
                  (swap! pages #(drop pagesiz %))
                  page)]
       (pprint x))
      (flush)
      (when (and (not (empty? @pages)) (not= end (read-line)))
        (cond 
          (and prompt? space?)
          (println "\nPress any key, or " end "to exit...\n")
          prompt?
          (println "Press any key, or " end "to exit...\n")
          space?
          (println "\n"))
        (flush)
        (recur)))))


#_(defn show-keystroke []
  (print "Enter a keystroke: ")
  (flush)
  ;; (let [term (Terminal/getTerminal)]
    ;; (.readCharacter term *in*))
  (let [cr (ConsoleReader.)
             keyint (.readCharacter cr)]
         (println (format "Got %d ('%c')!" keyint (char keyint)))))
