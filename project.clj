(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [data.deque "0.1.0"]
                 [org.clojure/core.async "1.3.610"]
                 [org.clojure/core.match "1.0.0"]
                 [io.helins/interval "1.0.0-beta0"]
                 [com.dean/interval-tree "0.1.2"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [fibonacci-heap-wrapper "0.1.0-SNAPSHOT"]
                 [criterium "0.4.6"]
                 [jline "2.11"]]

  :resource-paths ["resources"]
  :repl-options {:init-ns advent-of-code.core})
