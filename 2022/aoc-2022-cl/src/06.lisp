(defpackage aoc-2022-cl.day-06
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-06)

(defun first-unique-window (input window-size)
  (loop for i from 0 to (- (length input) window-size)
        for window = (subseq input i (+ window-size i))
          when (string= (nub window) window)
          return (+ i window-size)))

(defun solve ()
  (let ((input (str:from-file "./input/06.txt")))
    (format t "Part 1: ~A~%" (first-unique-window input 4))
    (format t "Part 2: ~A" (first-unique-window input 14))))
