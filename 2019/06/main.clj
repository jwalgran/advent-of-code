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
["K" "YOU"]
["I" "SAN"]
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
  ([dag moon] (orbit-seq dag moon nil))
  ([dag moon stop]
   (lazy-seq (if (= moon stop) nil (cons (dag moon) (orbit-seq dag (dag moon) stop)))))
  )

(def not-nil? (complement nil?))

(defn orbits
  ""
  ([dag moon] (orbits dag moon nil))
  ([dag moon stop]
   (take-while not-nil? (orbit-seq dag moon stop)))
  )

(defn count-orbits
  ""
  [dag moon]
  (count (orbits dag moon))
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

(defn common-orbits
  ""
  [dag m1 m2]
  (clojure.set/intersection (set (orbits dag m1)) (set (orbits dag m2)))
  )

(defn distance
  ""
  [dag m1 m2 junction]
  (+ -2 (count (orbits dag m1 junction)) (count (orbits dag m2 junction)))
  )


(defn distances
  ""
  [dag m1 m2]
  (map (partial distance dag m1 m2) (common-orbits dag m1 m2))
  )


(defn shortest-path-to-santa
  ""
  []
  (let [dag (reduce add-orbit {} (load-input))]
    (apply min (distances dag "YOU" "SAN"))
    )
  )
