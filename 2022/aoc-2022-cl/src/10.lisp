(defpackage aoc-2022-cl.day-10
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-10)

;;; Some more loop madness.

(defun solve-a ()
  (loop with x = 1
        with cycle = 0
        with result = 0
        with work =
                  (lambda (opscount)
                    (loop repeat opscount
                          do (incf cycle)
                          when (member cycle '(20 60 100 140 180 220))
                            do (incf result (* cycle x))))
        for line in (uiop:read-file-lines "./input/10.txt")
        for (optype arg) = (read-from-string (format nil "(~A)" line))
        when (eq optype 'addx)
          do (funcall work 2) and do (incf x arg)
        when (eq optype 'noop)
          do (funcall work 1)
        finally (return result)))

(defun print-pixels (pixels)
  (loop for pixel in pixels
        for i from 1
        do (format t "~A" pixel)
        when (zerop (mod i 40))
          do (format t "~%")))

(defun solve-b ()
  (loop with pixels = '()
        with x = 1
        with current-op = nil
        with lines = (uiop:read-file-lines "./input/10.txt")
        for cycle from 0
        do
           (push (if (member (mod cycle 40) (list (1- x) x (1+ x))) '\# '\.)
                 pixels)
        if (null current-op)
          do
             (let* ((line (pop lines))
                    (op (read-from-string (format nil "(~A)" line))))
               (when (eq 'addx (car op))
                 (setf current-op (lambda () (incf x (car (last op)))))))
        else
          do (funcall current-op) and do (setf current-op nil)

        when (and (null current-op) (null lines))
          do (print-pixels (reverse pixels)) and do (return t)))
