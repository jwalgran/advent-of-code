(ns aoc.day3.main
  (:require [clojure.data.csv :as csv])
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as s]))

(defn parse-movement
  "Parse a movement string into direction code and distance."
  [movement]
  [(subs movement 0 1) (edn/read-string (subs movement 1))]
  )

(defn parse-path
  "Parse a path vector into a seq of movement vectors."
  [path]
  (map parse-movement path)
  )

(defn load-input
  "Load  from an input file (default input.txt). Return an integer vector."
  ([] (load-input "input.txt"))
  ([file] (map parse-path (csv/read-csv (slurp file))))
  )

(defn movement-to-coordinates
  "Convert a direction vector to a seq of coordinates."
  [start movement]
  (let [[x y] start
        [direction distance] movement]
    (case direction
      "U" (map vector (repeat x) (range (+ y 1) (+ 1 y distance)))
      "D" (map vector (repeat x) (range (- y 1) (- y distance 1) -1))
      "L" (map vector (range (- x 1) (- x distance 1) -1) (repeat y))
      "R" (map vector (range (+ x 1) (+ 1 x distance) ) (repeat y))
      ))
  )

(defn path-to-coordinates
  "Convert a seq of direction vectors to a seq of coordinates"
  ([path] (path-to-coordinates [0 0] path))
  ([start path]
   (loop [s start
          p path
          c (list)]
     (if (empty? p)
       c
       (let [newc (movement-to-coordinates s (first p))]
         (recur (last newc) (rest p) (concat c newc))))
     )
   )
  )

(defn distance-to-coord
  "Find the distance of the first occurrence"
  [coord wire]
  (+ 1 (first (keep-indexed #(if (= coord %2) %1) wire)))
  )

(defn find-distances-to-intersections-in-input
  "Find the points where the paths in the input file intersect."
  []
  (let [wires (map path-to-coordinates (load-input))
        intersections (apply clojure.set/intersection (map set wires))]
    (for [i intersections]
      (map #(distance-to-coord i %1) wires))
    )
  )

(defn find-intersections-in-input
  "Find the points where the paths in the input file intersect."
  []
  (apply clojure.set/intersection (map set (map path-to-coordinates (load-input))))
  )

(defn abs [n] (max n (- n)))

(defn find-closest-intersection-in-input-manhattan
  "Find the answer to part 1."
  []
  (apply min (map (fn [[x y]] (+ (abs x) (abs y))) (find-intersections-in-input)))
  )

(defn find-closest-intersection-in-input-along-wire
  "Find the answer to part 2."
  []
  (apply min (map (fn [[x y]] (+ (abs x) (abs y)))
                  (find-distances-to-intersections-in-input)))
  )
