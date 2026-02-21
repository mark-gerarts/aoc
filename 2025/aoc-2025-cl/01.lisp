(in-package :aoc-2025-cl)

(defun parse-line (line)
  (let ((direction (case (char line 0) (#\L :left) (#\R :right)))
        (amount (parse-integer line :start 1)))
    (cons direction amount)))

(defun parse-input ()
  (sr:~>> "input/01.txt"
          uiop:read-file-lines
          (remove-if #'ax:emptyp)
          (mapcar #'parse-line)))

(defun turn-dial-one-step (dial direction)
  (sr:~> direction
         (tr:match (:left -1) (:right 1))
         (+ dial)
         (mod 100)))

(defun run-input ()
  (loop with dial = 50
        for (direction . amount) in (parse-input)
        collect (loop repeat amount
                      do (setf dial (turn-dial-one-step dial direction))
                      collect dial)))

(loop for sequence in (run-input)
      sum (count-if #'zerop sequence) into total-part-2
      when (zerop (ax:lastcar sequence))
        count it into total-part-1
      finally (format t "Part 1: ~D~%Part 2: ~D~%" total-part-1 total-part-2))
