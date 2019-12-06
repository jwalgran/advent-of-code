(ns aoc.day6.main
  (:require [clojure.string :as s]))

(def test-orbits [
["COM" "B"]
["B" "C"]
["C" "D"]
["D" "E"]
["E" "F"]
["B" "G"]
["G" "H"]
["D" "I"]
["E" "J"]
["J" "K"]
["K" "L"]
           ])

(defn load-input
  "Load a program from an input file (default input.txt). Return a vector of
  planet pair vectors."
  ([] (load-input "input.txt"))
  ([file] (map #(s/split % #"\)") (s/split-lines (slurp file))) )
  )

(defn add-orbit
  ""
  [dag [center moon]]
  (assoc dag moon center))

(defn orbit-seq
  ""
  [dag moon]
  (lazy-seq (cons (dag moon) (orbit-seq dag (dag moon))))
  )

(def not-nil? (complement nil?))

(defn count-orbits
  ""
  [dag moon]
  (count (take-while not-nil? (orbit-seq dag moon)))
  )

(defn count-all-orbits
  ""
  [orbits]
  (let [dag (reduce add-orbit {} orbits)]
    (reduce + (map (partial count-orbits dag) (keys dag)))
    )
  )

(defn count-all-orbits-in-input
  ""
  []
  (count-all-orbits (load-input))
  )
