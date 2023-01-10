(ns aoc-2022-clj.01
  (:require [clojure.string :as str]))

(defn parse-input [raw-input]
  (as-> raw-input input
    (str/split input #"\n\n")
    (map str/split-lines input)
    (map (fn [group] (map #(Integer/parseInt %) group)) input)
    (map #(reduce + %) input)
    (sort > input)))

(defn solve []
  (let [input (parse-input (slurp "./input/01.txt"))]
    (println (str "Part A: " (first input)))
    (println (str "Part B: " (apply + (take 3 input))))))
