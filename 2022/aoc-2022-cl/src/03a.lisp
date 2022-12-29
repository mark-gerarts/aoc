(defpackage day-03a
  (:use :cl))
(in-package :day-03a)

(defun parse-line (line)
  (let* ((middle (floor (length line) 2)))
    (mapcar
        (lambda (string) (coerce string 'list))
        (list (subseq line 0 middle) (subseq line middle)))))

(defun get-shared-item (left right)
  (car (intersection left right)))

(defun get-priority (char)
  (let ((ascii-value (char-code char)))
    (if (< ascii-value 97)
        (- ascii-value 38)
        (- ascii-value 96))))

(defun solve ()
  (with-open-file (stream "./input/03.txt")
    (loop for line = (read-line stream nil)
          while line
          for (left right) = (parse-line line)
          for shared-item = (get-shared-item left right)
            sum (get-priority shared-item))))
