(defpackage aoc-2022-cl.day-03b
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-03b)

(defun get-shared-item (a b c)
  (car (intersection (intersection a b) c)))

(defun get-priority (char)
  (let ((ascii-value (char-code char)))
    (if (< ascii-value 97)
        (- ascii-value 38)
        (- ascii-value 96))))

(defun solve ()
  (let* ((input (str:lines (str:from-file "./input/03.txt")))
         (input (mapcar (lambda (line) (coerce line 'list)) input))
         (groups (serapeum:batches input 3)))
    (loop for group in groups
          for shared-item = (apply #'get-shared-item group)
            summing (get-priority shared-item))))
