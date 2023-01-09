(defpackage aoc-2022-cl.day-10
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-10)

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
