(in-package :aoc-2025-cl)

(defun parse-input ()
  (sr:~>
   (loop for rownum from 0
         for line in (uiop:read-file-lines "input/04.txt")
         collect (loop for char across line collect char))
   (make-array (list (length _) (length (first _)))
               :initial-contents _)))

(defun neighbor-positions (position array)
  (loop with (x y) = position
        for dx from -1 to 1
        append (loop for dy from -1 to 1
                     for neighbor = (list (+ x dx) (+ y dy))
                     when (and (not (equal neighbor position))
                               (apply #'array-in-bounds-p array neighbor))
                       collect neighbor)))

(defun neighbor-symbols (position array)
  (loop for neighbor in (neighbor-positions position array)
        collect (apply #'aref array neighbor)))

(defun can-remove (position array)
  (and (char= #\@ (apply #'aref array position))
       (< (count #\@  (neighbor-symbols position array)) 4)))

(defun count-paper (grid)
  (loop for i from 0 below (array-total-size grid)
        for element = (row-major-aref grid i)
        when (char= element #\@)
          count it))

(defun remove-accessible-paper (grid)
  (loop with (rows cols) = (array-dimensions grid)
        with new-grid = (ax:copy-array grid)
        for row from 0 below rows
        do (loop for col from 0 below cols
                 when (can-remove (list row col) grid)
                   do (setf (aref new-grid row col) #\.))
        finally (return new-grid)))

(defun remove-accessible-paper-until-stabilized (grid)
  (loop for current = grid then next
        for next = (remove-accessible-paper current)
        until (equalp current next)
        finally (return next)))

(defparameter *grid* (parse-input))
(defparameter *original-count* (count-paper *grid*))

(format t "Part 1: ~A~%"
        (- *original-count*
           (count-paper (remove-accessible-paper *grid*))))

(format t "Part 2: ~A~%"
        (- *original-count*
           (count-paper (remove-accessible-paper-until-stabilized *grid*))))
