(defpackage aoc-2022-cl.day-04b
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-04b)

(defun parse-range (input)
  "Converts the string '3-8' in a list of numbers from 3 to 8 inclusive."
  (arrows:-<>> input
    (str:split #\-)
    (mapcar #'parse-integer)
    (apply (lambda (start end) (range start (1+ end))))
    (coerce <> 'list)))

(defun parse-line-to-ranges (line)
  (mapcar #'parse-range (str:split #\, line)))

(defun solve ()
  (with-open-file (stream "./input/04.txt")
    (loop for line = (read-line stream nil)
          while line
          for (a b) = (parse-line-to-ranges line)
            when (intersection a b)
            count it)))
