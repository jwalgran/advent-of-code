(ns aoc.main
  (:require [clojure.edn :as edn])
  (:require [clojure.java.io :as io]))

(defn fuel-required
  "The fuel required to lift a given mass"
  [mass]
  (let [fuel-mass (- (quot mass 3) 2)]
    (if (<= fuel-mass 0) 0 fuel-mass))
  )

(defn fuel-seq
  "Create a sequence of the fuel required for a mass and the additional
  recursive fuel required"
  [mass]
  (let [fuel-mass (fuel-required mass)]
    (lazy-seq (cons fuel-mass (fuel-seq fuel-mass))))
  )

(defn fuel-sum
  "The total amount of fuel required for a mass and its recursive fuel"
  [mass]
  (reduce + (take-while pos? (fuel-seq mass)))
  )

(defn total-fuel-required
  "Read masses from file and return total fuel required"
  ([] (total-fuel-required "input.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (reduce + (map (comp fuel-sum edn/read-string) (line-seq rdr)))))
  )
