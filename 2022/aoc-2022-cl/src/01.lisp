(defpackage aoc-2022-cl.day-01
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-01)

(defun parse-input (input)
  (loop for group in (cl-ppcre:split "\\n\\n" input)
        for split-group = (cl-ppcre:split "\\n" group)
        collect (reduce #'+ split-group :key #'parse-integer) into totals
        finally (return (sort totals #'>))))

(defun solve ()
  (let ((calorie-totals (parse-input (uiop:read-file-string "./input/01.txt"))))
    (format t "Part A: ~A~%" (first calorie-totals))
    (format t "Part B: ~A~%" (apply #'+ (subseq calorie-totals 0 3)))))
