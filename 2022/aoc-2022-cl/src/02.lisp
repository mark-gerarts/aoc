(defpackage aoc-2022-cl.day-02b
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-02b)

(defparameter game-rules
  (dict
   :rock
   (dict :against
         (dict :rock :draw
               :paper :lose
               :scissors :win)
         :score 1)
   :paper
   (dict :against
         (dict :rock :win
               :paper :draw
               :scissors :lose)
         :score 2)
   :scissors
         (dict :against
               (dict :rock :lose
                     :paper :win
                     :scissors :draw)
               :score 3)))

(defun score (my-shape opp-shape)
  (let* ((result (@ game-rules my-shape :against opp-shape))
         (score-result (ecase result (:win 6) (:draw 3) (:lose 0)))
         (score-shape (@ game-rules my-shape :score)))
    (+ score-result score-shape)))

(defun score-for-line-a (line)
  (let ((opp-shape (ecase (char line 0)
                          (#\A :rock)
                          (#\B :paper)
                          (#\C :scissors)))
        (my-shape (ecase (char line 2)
                         (#\X :rock)
                         (#\Y :paper)
                         (#\Z :scissors))))
    (score my-shape opp-shape)))

(defun score-for-line-b (line)
  (let* ((opp-shape (ecase (char line 0)
                           (#\A :rock)
                           (#\B :paper)
                           (#\C :scissors)))
         (outcome (ecase (char line 2)
                         (#\X :win)
                         (#\Y :draw)
                         (#\Z :lose)))
         (my-shape (arrows:-<>>
                    (@ game-rules opp-shape :against)
                    (flip-hash-table)
                    (@ <> outcome))))
    (score my-shape opp-shape)))

(defun solve-part (score-fn)
  (arrows:->>
   (uiop:read-file-lines "./input/02.txt")
   (mapcar score-fn)
   (apply #'+)))

(defun solve ()
  (format t "Part A: ~A~%" (solve-part #'score-for-line-a))
  (format t "Part B: ~A~%" (solve-part #'score-for-line-b)))
