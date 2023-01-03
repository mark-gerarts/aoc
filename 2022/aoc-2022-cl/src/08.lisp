(defpackage aoc-2022-cl.day-08
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-08)

(defun parse-input (input)
  "Parse the input in a list of pairs (id . height)"
  (loop with id = 0
        for c across input
        for height = (digit-char-p c)
          when height
        collect (cons id height) and do (incf id)))

(defun map-width (map)
  (floor (sqrt (length map))))

(defun look-at (map direction)
  "Rearranges the trees in a map such that they are ordened as if you were
  looking into the given direction."
  (let ((width (map-width map)))
    (trivia:match direction
      (:right
       (loop with result = (make-list width)
             for (id . height) in map
             do (push (cons id height) (nth (floor id width) result))
             finally (return result)))
      (:up
       (loop with result = (make-list width)
             for (id . height) in map
             do (push (cons id height) (nth (mod id width) result))
             finally (return result)))
      (:down (mapcar #'reverse (look-at map :up)))
      (:left (mapcar #'reverse (look-at map :right))))))

(defun solve-a ()
  (loop with map = (parse-input (str:from-file "./input/08.txt"))
        with results = '()
        for direction in '(:up :down :left :right)
        for rows = (look-at map direction)
        do (loop for row in rows
                 do (loop with last-height = -1
                          for (id . height) in row
                            when (> height last-height)
                          do (pushnew id results) and
                          do (setf last-height height)))
        finally (return (length results))))

(defun solve-b ()
  "Loop madness >.>"
  (loop with map = (parse-input (str:from-file "./input/08.txt"))
        with results = (loop for (id . _) in map collect (cons id 1))
        for direction in '(:up :down :left :right)
        for rows = (look-at map direction)
        do (loop for row in rows
                 do (loop for (id . height) in row
                          for i from 0
                          for other-heights = (subseq row i)
                          for score = (loop named inner-loop
                                          with tree-count = -1
                                          for (_ . other-height) in other-heights
                                          do (incf tree-count)
                                            when (and (/= tree-count 0) (>= other-height height))
                                          do (return-from inner-loop tree-count)
                                          finally (return-from inner-loop tree-count))
                          do (rplacd
                               (assoc id results)
                               (* score (cdr (assoc id results))))))
        finally (return
                  (loop for (id . score) in results
                          maximizing score))))

(defun solve ()
  (format t "Part A: ~A~%" (solve-a))
  (format t "Part B: ~A~%" (solve-b)))
