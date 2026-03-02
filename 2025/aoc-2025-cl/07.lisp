(in-package :aoc-2025-cl)

(defun href-add (hash-table key value)
  (setf (sr:href hash-table key)
        (+ (sr:href-default 0 hash-table key) value)))

;; Loop macro goes brrr.
(loop with num-splits = 0
      with beam-counts = (sr:dict)
      for line in (sr:lines (uiop:read-file-string "input/07.txt"))
      do (loop for x from 0
               for char across line
               for beam-above = (sr:href beam-counts x)
               when (char= #\S char)
                 do (setf (sr:href beam-counts x) 1)
               when (and beam-above (char= #\^ char))
                 do (incf num-splits)
                 and do (sr:pophash x beam-counts)
                 and do (href-add beam-counts (1- x) beam-above)
                 and do (href-add beam-counts (1+ x) beam-above))
      finally (progn
                (format t "Part 1: ~A~%" num-splits)
                (format t "Part 2: ~A~%"
                        (sr:hash-fold (sr:op (+ _2 _3)) 0 beam-counts))))
