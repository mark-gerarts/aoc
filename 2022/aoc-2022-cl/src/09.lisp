(defpackage aoc-2022-cl.day-09
  (:use :cl :alexandria :serapeum))
(in-package aoc-2022-cl.day-09)

(deftype pos () '(satisfies pos-p))

(-> pos-p (t) boolean)
(defun pos-p (thing)
  (and (listp thing)
       (= (length thing) 2)
       (fixnump (first thing))
       (fixnump (second thing))))

(-> add (pos pos) pos)
(defun add (pos-a pos-b)
  (list (+ (first pos-a) (first pos-b))
        (+ (second pos-a) (second pos-b))))

(-> diff (pos pos) pos)
(defun diff (pos-a pos-b)
  (list (- (first pos-a) (first pos-b))
        (- (second pos-a) (second pos-b))))

(defun get-segment-pos (segment prev)
  "Calculates the new position of a tail segment given the previous segment."
  (add segment
       (trivia:match (diff prev segment)
         ;; Up/down/left/right
         ('(0 2) '(0 1))
         ('(0 -2) '(0 -1))
         ('(2 0) '(1 0))
         ('(-2 0) '(-1 0))
         ;; Diagonal (chess horse)
         ('(1 2) '(1 1))
         ('(-1 2) '(-1 1))
         ('(1 -2) '(1 -1))
         ('(-1 -2) '(-1 -1))
         ('(2 1) '(1 1))
         ('(2 -1) '(1 -1))
         ('(-2 1) '(-1 1))
         ('(-2 -1) '(-1 -1))
         ;; Diagonal (regular)
         ('(2 2) '(1 1))
         ('(2 -2) '(1 -1))
         ('(-2 2) '(-1 1))
         ('(-2 -2) '(-1 -1))
         ;; T/H are on top of eachother or already correctly
         ;; positioned.
         (_ '(0 0)))))

(-> move-rope (list pos) list)
(defun move-rope (rope step)
  (loop for segment in rope
        for i from 0
          if (zerop i)
          ;; Update the head position.
        collect (add segment step) into new-rope
          else
          ;; Update tail segments based on the previous one.
        collect (get-segment-pos segment (nth (1- i) new-rope)) into new-rope
        finally (return new-rope)))

(defun parse-input ()
  (loop for line in (uiop:read-file-lines "./input/09.txt")
        collect (trivia:match line
                  ((trivia.ppcre:ppcre "U (\\d+)" steps) `((0 1) ,steps))
                  ((trivia.ppcre:ppcre "D (\\d+)" steps) `((0 -1) ,steps))
                  ((trivia.ppcre:ppcre "L (\\d+)" steps) `((-1 0) ,steps))
                  ((trivia.ppcre:ppcre "R (\\d+)" steps) `((1 0) ,steps)))))

(-> solve (fixnum) fixnum)
(defun solve (num-segments)
  (loop with rope = (make-list num-segments :initial-element '(0 0))
        with seen = '((0 0))
        for (pos steps) in (parse-input)
        do (loop repeat (parse-integer steps)
                 do (setf rope (move-rope rope pos))
                 do (setf seen (adjoin (car (last rope)) seen :test #'equal)))
        finally (return (length seen))))

(defun solve-both ()
  (format t "Part A: ~A~%" (solve 2))
  (format t "Part B: ~A~%" (solve 10)))
