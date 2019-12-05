(ns aoc.day5.main
  (:require [clojure.edn :as edn])
  (:require [clojure.string :as s]))

(defn load-input
  "Load a program from an input file (default input.txt). Return an integer vector."
  ([] (load-input "input.txt"))
  ([file] (into (vector)
                (map edn/read-string
                     (s/split (s/trim-newline (slurp file)) #","))))
  )

(defn step
  "Run instruction at pointer and return the new program and new pointer index."
  [program pointer]
  (if (== (program pointer) 99)
    [program nil]
    (let [xi (program (+ pointer 1))
          x (program xi)
          yi (program (+ pointer 2))
          y (program yi)
          i (program (+ pointer 3))
          op (case (program pointer)
               1 +
               2 *)]
      [(assoc program i (op x y)) (+ pointer 4)])
    )
  )

(defn exec
  "Run the program starting from the specific pointer (default 0) and return the
  first element of the completed program after it halts"
  ([program] (exec program 0))
  ([program pointer]
   (loop [pr program
          pt pointer]
     (let [[npr npt] (step pr pt)]
       (if (nil? npt)
           npr
           (recur npr npt))))
   )
  )

(defn run-patch
  "Run the input.txt program after applying the patch from the problem instructions."
  ([] (run-patch 12 2))
  ([noun verb]
   (let [p (load-input)
         newp (assoc (assoc p 1 noun) 2 verb)]
     ((exec newp) 0)))
  )

(defn find-patch
  "Find the noun and verb that result in 19690720"
  []
  (let [patch-seq (for [noun (range 100)
                        verb (range 100)]
                    [noun verb (run-patch noun verb)])]
    (first (drop-while (fn [[_ _ result]] (not= 19690720 result)) patch-seq)))
  )
