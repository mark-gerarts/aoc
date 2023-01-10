(ns aoc-2022-clj.02
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as str]))

(def game-rules
  {:rock
   {:against {:rock :draw :paper :lose :scissors :win}
    :score 1}
   :paper
   {:against {:rock :win :paper :draw :scissors :lose}
    :score 2}
   :scissors
   {:against {:rock :lose :paper :win :scissors :draw}
    :score 3}})

(defn score [opp-shape my-shape]
  (let [result (get-in game-rules [my-shape :against opp-shape])
        score-result (result {:lose 0 :draw 3 :win 6})
        score-shape (get-in game-rules [my-shape :score])]
    (+ score-result score-shape)))

(defn get-score-for-input-line-a [line]
  (let [opp-shape (get {\A :rock \B :paper \C :scissors} (nth line 0))
        my-shape (get {\X :rock \Y :paper \Z :scissors} (nth line 2))]
    (score opp-shape my-shape)))

(defn get-score-for-input-line-b [line]
  (let [opp-shape (get {\A :rock \B :paper \C :scissors} (nth line 0))
        outcome (get {\X :win \Y :draw \Z :lose} (nth line 2))
        my-shape (->> (opp-shape game-rules)
                      (:against)
                      (map-invert)
                      (outcome))]
    (score opp-shape my-shape)))

(defn solve-part [score-fn]
  (->> (slurp "./input/02.txt")
       (str/split-lines)
       (transduce (map score-fn) +)))

(defn solve []
  (println (str "Part A: "
                (solve-part get-score-for-input-line-a)))
  (println (str "Part B: "
                (solve-part get-score-for-input-line-b))))
