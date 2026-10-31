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
  machine)

(defvar *test-line* "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
(solve-machine (parse-line *test-line*))
