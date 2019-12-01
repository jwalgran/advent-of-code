(ns aoc.main
  (:require [clojure.edn :as edn])
  (:require [clojure.java.io :as io]))

(defn fuel-required
  "The fuel required to lift a given mass"
  [mass]
  (- (quot mass 3) 2)
  )

(defn total-fuel-required
  "Read masses from file and return total fuel required"
  ([] (total-fuel-required "input.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (reduce + (map (comp fuel-required edn/read-string) (line-seq rdr)))))
  )
