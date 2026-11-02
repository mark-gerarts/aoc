(in-package :aoc-2025-cl)

(setf lp:*solver* glpk:glpk-solver)

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
           (sr:~>> (rest (butlast parts))
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
  (if (eql :on light) :off :on))

(defun apply-wiring (wiring lights)
  (loop for light in lights
        for i from 0
        if (member i wiring)
          collect (toggle light)
        else
          collect light))

(defun configure-indicator-lights (machine)
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
                     do (return-from configure-indicator-lights search-depth)
                   do (setf frontier (fset:with frontier new-state))))))

(defun configure-joltage-levels (machine)
  "Model the problem as a set of linear equations, which we can pass to a
solver. E.g. for a line with buttons (0) (1) (0,1) and target [2;4],
we create the following equations:

- b0 + b2 = 2
- b1 + b2 = 4
- total = b0 + b1

where bi is the number of times button i is pressed. Then we solve while
minimizing `total`."
  (let*
      ((button-vars
         (loop for i from 0 below (length (getf machine :wirings))
               collect (intern (format nil "b~D" i))))
       (target `(min (= total (+ ,@button-vars))))
       (button-wirings
         (loop for joltage-level in (getf machine :joltage-reqs)
               for index from 0
               for buttons
                 = (loop for wiring in (getf machine :wirings)
                         for button-var in button-vars
                         when (member index wiring) collect button-var)
               collect `(= (+ ,@buttons) ,joltage-level))))
    (eval `(lp:with-solved-problem  (,target (>= (+ ,@button-vars) 0)
                                             ,@button-wirings
                                             (integer ,@button-vars))
             (round total)))))

(defparameter *machines*
  (sr:~>> "input/10.txt"
          uiop:read-file-lines
          (remove-if #'sr:blankp)
          (mapcar #'parse-line)))

(sr:~>> *machines*
        (mapcar #'configure-indicator-lights)
        (reduce #'+)
        (format t "Part 1: ~A~%"))

(sr:~>> *machines*
        (mapcar #'configure-joltage-levels)
        (reduce #'+)
        (format t "Part 2: ~A~%"))
