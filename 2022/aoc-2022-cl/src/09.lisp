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

(deftype state () '(satisfies state-p))

(-> state-p (t) boolean)
(defun state-p (thing)
  (and (hash-table-p thing)
       (@ thing :pos-head)
       (@ thing :pos-tail)
       (@ thing :seen)
       t))

(-> add (pos pos) pos)
(defun add (pos-a pos-b)
  (list (+ (first pos-a) (first pos-b))
        (+ (second pos-a) (second pos-b))))

(-> diff (pos pos) pos)
(defun diff (pos-a pos-b)
  (list (- (first pos-a) (first pos-b))
        (- (second pos-a) (second pos-b))))

(-> move-tail (pos pos) pos)
(defun move-tail (pos-head pos-tail)
  (add pos-tail (trivia:match (diff pos-head pos-tail)
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
                  ;; T/H are on top of eachother or already correctly
                  ;; positioned.
                  (_ '(0 0)))))

;; Use state hash table instead, returning new dict?
(-> apply-step (state pos) state)
(defun apply-step (state step)
  (let* ((new-pos-head (add (@ state :pos-head) step))
         (new-pos-tail (move-tail new-pos-head (@ state :pos-tail)))
         (new-seen (adjoin new-pos-tail (@ state :seen) :test #'equal)))
    (dict :pos-head new-pos-head
          :pos-tail new-pos-tail
          :seen new-seen)))

(defun parse-input ()
  (loop for line in (uiop:read-file-lines "./input/09.txt")
        collect (trivia:match line
                  ((trivia.ppcre:ppcre "U (\\d+)" steps) `((0 1) ,steps))
                  ((trivia.ppcre:ppcre "D (\\d+)" steps) `((0 -1) ,steps))
                  ((trivia.ppcre:ppcre "L (\\d+)" steps) `((-1 0) ,steps))
                  ((trivia.ppcre:ppcre "R (\\d+)" steps) `((1 0) ,steps)))))

(defun solve-a ()
  (loop with state = (dict :pos-head '(0 0)
                           :pos-tail '(0 0)
                           :seen '((0 0)))
        for (pos steps) in (parse-input)
        do (loop repeat (parse-integer steps)
                 for new-state = (apply-step state pos)
                 do (setf state new-state))
        finally (return (length (@ state :seen)))))

(defun solve ()
  (format t "Part A: ~A~%" (solve-a)))
