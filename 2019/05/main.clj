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

(def cmd-lengths
     "How far the pointer should advance after executing a command"
     {
      1 4
      2 4
      3 2
      4 2
      99 1
      })

(defn parse-cmd
  "Create an attribute map from the command at the specified pointer."
  [program pointer]
  (let [cmd-str (format "%04d" (program pointer))
        cmd-code (edn/read-string (subs cmd-str 2))
        cmd-len (cmd-lengths cmd-code)
        cmd (subvec program pointer (+ pointer cmd-len))]
    (if (= 99 cmd-code)
      {
       :cmd-code cmd-code
       :increment 0
       }
      {
       :cmd-code cmd-code
       :increment cmd-len
       :params (subvec cmd 1 (- cmd-len 1))
       :out-addr (program (+ pointer (- cmd-len 1)))
       :modes (vec
               (map #(case % \1 :immediate \0 :position)
                    (reverse (subvec (vec cmd-str) 0 (- cmd-len 2)))))
       }
      )
    )
  )

(defn step
  "Evaluate the program statement at the specified pointer"
  ([program pointer]
   (step program pointer (repeat nil) println)
   )
  (
   [program pointer input-seq output-fn]
   (let [cmd (parse-cmd program pointer)
         values (map (fn [[param mode]]
                       (case mode
                         :immediate param
                         :position (program param)))
                     (map vector (cmd :params) (cmd :modes)))
         ]
     (let [result (case (cmd :cmd-code)
                    1 (assoc program (cmd :out-addr) (apply + values))
                    2 (assoc program (cmd :out-addr) (apply * values))
                    3 (assoc program (cmd :out-addr) (first input-seq))
                    4 (do (output-fn (program (cmd :out-addr))) program)
                    99 program
                    )
           ]
       [result (+ pointer (cmd :increment))]
       )
     )
   )
  )

(defn exec
  "Run the program starting from the specific pointer (default 0) and return the
  first element of the completed program after it halts"
  ([program] (exec program 0))
  ([program pointer] (exec program pointer (repeat nil)))
  ([program pointer input-seq] (exec program pointer input-seq println))
  ([program pointer input-seq output-fn]
   (loop [pr program
          pt pointer]
     (let [[npr npt] (step pr pt input-seq output-fn)]
       (if (= pt npt)
         :halt
         (recur npr npt))))
   )
  )

(defn solve-puzzle
  "Run the input as specified in the instructions"
  []
  (exec (load-input) 0 (repeat 1)))
