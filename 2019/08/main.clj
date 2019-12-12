(ns aoc.day8.main
  (:require [clojure.string :as s]))

(defn load-input
  "Load a program from an input file (default input.txt). Return an integer vector."
  ([] (load-input "input.txt"))
  ([file] (into (vector)
                (map (comp #(Integer/parseInt %) #(str %))
                     (seq (s/trim-newline (slurp file))))))
  )

(def width 25)
(def height 6)

(defn layers
  ""
  [w h input]
  (partition (* w h) input))

(defn input-layers
  ""
  []
  (layers width height (load-input)))

(defn min-zero-layer
  ""
  [layers]
  (first (sort-by (fn [[i c]] ((frequencies c) 0))
                  (map vector (range) (input-layers))))
  )

(defn solve-part-1
  ""
  []
  (let [[i layer] (min-zero-layer (input-layers))
        counts (frequencies layer)]
    (* (counts 1) (counts 2)))
  )

(defn merge-layers
  ""
  [a b]
  (map (fn [[aa bb]] (if (= aa 2) bb aa)) (map vector a b))
  )

(defn solve-part-2
  ""
  []
  (map s/join
       (partition width
                  (map (fn [x] (if (= 0 x) " " "X"))
                       (reduce merge-layers (input-layers)))))
  )
