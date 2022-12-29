(defpackage day-04b
  (:use :cl))
(in-package :day-04b)

(defun range (start end)
  "Creates a list of numbers from start to end, inclusive."
  (loop for n from start to end collect n))

(defun parse-range (input)
  "Converts the string '3-8' in a list of numbers from 3 to 8 inclusive."
  (arrows:->> input
    (str:split #\-)
    (mapcar #'parse-integer)
    (apply #'range)))

(defun parse-line-to-ranges (line)
  (mapcar #'parse-range (str:split #\, line)))

(defun solve ()
  (with-open-file (stream "./input/04.txt")
    (loop for line = (read-line stream nil)
          while line
          for (a b) = (parse-line-to-ranges line)
            when (intersection a b)
            count it)))
