(in-package :aoc-2025-cl)

(sr:~>> "input/10.test"
        uiop:read-file-lines
        (remove-if #'sr:blankp))

(defun parse-number-list (number-list)
  (sr:~>> number-list
          (string-trim "{()}")
          (sr:split-sequence #\,)
          (mapcar #'parse-integer)))

(defun parse-line (line)
  (let* ((parts
           (sr:split-sequence #\Space line))
         (light-diagram
           (sr:~>> (first parts)
                   (string-trim "[]")
                   (map 'list (sr:op (if (char= _ #\.) :off :on)))))
         (wirings
           (sr:~>> (cdr (butlast parts))
                   (mapcar 'parse-number-list)))
         (joltage-reqs
           (sr:~>> (ax:lastcar parts)
                   parse-number-list))
         (lights
           (make-list (length light-diagram) :initial-element :off)))
    (list :light-diagram light-diagram
          :wirings wirings
          :joltage-reqs joltage-reqs
          :lights lights)))

(defun toggle (light)
  (if (eq :on light) :off :on))

(defun apply-wiring (wiring lights)
  (loop for light in lights
        for i from 0
        if (member i wiring)
          collect (toggle light)
        else
          collect light))

(defun solve-machine (machine)
  "Basic BFS."
  (loop with seen-states = (fset:set (getf machine :lights))
        with frontier = seen-states
        with goal-state = (getf machine :light-diagram)
        with wirings = (getf machine :wirings)
        for search-depth from 1
        do (fset:do-set (state frontier)
             (loop for wiring in wirings
                   for new-state = (apply-wiring wiring state)
                   when (equal new-state goal-state)
                     do (return-from solve-machine search-depth)
                   do (setf frontier (fset:with frontier new-state))))))

(sr:~>> "input/10.txt"
        uiop:read-file-lines
        (remove-if #'sr:blankp)
        (mapcar #'parse-line)
        (mapcar #'solve-machine)
        (reduce #'+)
        (format t "~A~%"))
