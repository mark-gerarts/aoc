(defpackage day-02a
  (:use :cl))
(in-package :day-02a)

(defun get-game-rules (shape)
  (trivia:match shape
    (:rock
     (serapeum:dict
       :draw :rock
       :win-from :scissors
       :lose-from :paper))
    (:paper
     (serapeum:dict
       :draw :paper
       :win-from :rock
       :lose-from :scissors))
    (:scissors
     (serapeum:dict
       :draw :scissors
       :win-from :paper
       :lose-from :rock))))

(defun get-my-shape (opp-shape outcome)
  (gethash
    (trivia:match outcome
      (:win-from :lose-from)
      (:lose-from :win-from)
      (:draw :draw))
    (get-game-rules opp-shape)))

(defun get-score (outcome shape)
  (let ((score-outcome
         (trivia:match outcome
           (:win-from 6)
           (:draw 3)
           (_ 0)))
        (score-shape
         (trivia:match shape
           (:rock 1)
           (:paper 2)
           (_ 3))))
    (+ score-outcome score-shape)))

(defun parse-to-shape (input)
  (trivia:match input
    (#\A :rock)
    (#\B :paper)
    (#\C :scissors)))

(defun parse-to-outcome (input)
  (trivia:match input
    (#\X :lose-from)
    (#\Y :draw)
    (#\Z :win-from)))

(defun parse-line (line)
  (list
   (parse-to-shape (char line 0))
   (parse-to-outcome (char line 2))))

(with-open-file (stream "./input/02.txt")
  (loop for line = (read-line stream nil)
        while line
        for (opp-shape outcome) = (parse-line line)
        for my-shape = (get-my-shape opp-shape outcome)
          sum (get-score outcome my-shape)))
