(defpackage aoc-2022-cl.day-02a
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-02a)

(defun calculate-points (opp-shape my-shape)
  (let ((score-outcome
         (cond
          ((eq opp-shape my-shape) 3)
          ((and (eq opp-shape :rock) (eq my-shape :scissors)) 0)
          ((and (eq opp-shape :paper) (eq my-shape :rock)) 0)
          ((and (eq opp-shape :scissors) (eq my-shape :paper)) 0)
          (t 6)))
        (score-shape
         (trivia:match my-shape
           (:rock 1)
           (:paper 2)
           (:scissors 3))))
    (+ score-outcome score-shape)))

(defun parse-to-shape (input)
  (trivia:match input
    ((or #\A #\X) :rock)
    ((or #\B #\Y) :paper)
    ((or #\C #\Z) :scissors)))

(defun parse-line (line)
  (mapcar #'parse-to-shape (list (char line 0) (char line 2))))

(defun solve ()
  (with-open-file (stream "./input/02.txt")
    (loop for line = (read-line stream nil)
          while line
          for (opp-shape my-shape) = (parse-line line)
            sum (calculate-points opp-shape my-shape))))
