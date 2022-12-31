(defpackage aoc-2022-cl.day-04a
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-04a)

(defun parse-range (input)
  "Converts the string '3-8' in a list of numbers from 3 to 8 inclusive."
  (arrows:-<>> input
    (str:split #\-)
    (mapcar #'parse-integer)
    (apply (lambda (start end) (range start (1+ end))))
    (coerce <> 'list)))

(defun parse-line-to-ranges (line)
  (mapcar #'parse-range (str:split #\, line)))

(defun contains-fully (a b)
  (eq (length (intersection a b)) (length b)))

(defun solve ()
  (with-open-file (stream "./input/04.txt")
    (loop for line = (read-line stream nil)
          while line
          for (a b) = (parse-line-to-ranges line)
            when (or (contains-fully a b) (contains-fully b a))
            count it)))
