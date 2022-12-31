(defpackage aoc-2022-cl.day-01
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-01)

(defun part-a ()
  (let ((input (str:from-file "./input/01.txt")))
    (apply #'max (calorie-totals input))))

(defun part-b ()
  (let* ((input (str:from-file "./input/01.txt"))
         (totals (calorie-totals input)))
    (get-sum-top-three totals)))

(defun calorie-totals (input)
  (loop for group in (cl-ppcre:split "\\n\\n" input)
        for split-group = (cl-ppcre:split "\\n" group)
        collect (reduce #'+ split-group :key #'parse-integer)))

(defun get-sum-top-three (numbers)
  (let* ((sorted (sort numbers #'>))
         (maxima (subseq sorted 0 3)))
    (apply #'+ maxima)))
