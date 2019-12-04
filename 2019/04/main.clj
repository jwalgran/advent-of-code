(ns aoc.day4.main
  (:require [clojure.edn :as edn])
  )

(defn digit-seq
  "A left-to-right sequence of digits from an integer."
  [number]
  (map (comp edn/read-string str) (seq (str number)))
  )

(defn digit-pairs
  "A sequence of pairs of digits in a number using a sliding window."
  [number]
  (partition 2 1 (digit-seq number))
  )


(defn digits-increase?
  "True if the digits in a number increase or stay the same from left-to-right."
  [number]
  (every? (fn [[a b]] (<= a b)) (digit-pairs number))
  )

(def not-nil? (complement nil?))

(defn digits-repeat?
  "True if at least one digit in the number repeats."
  [number]
  (not-nil? (some (fn [[a b]] (= a b)) (digit-pairs number)))
  )

(defn count-passwords
  "Solve the problem."
  []
  (count (->> (range 284639 748759)
              (filter digits-increase?)
              (filter digits-repeat?))))
