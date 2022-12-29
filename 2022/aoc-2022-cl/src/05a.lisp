(defpackage day-05a
  (:use :cl))
(in-package :day-05a)

(defun parse (input)
  (let ((input-and-instructions (cl-ppcre:split "\\n\\n" input)))
    (values
      (parse-setup (car input-and-instructions))
      (parse-instructions (cadr input-and-instructions)))))

(defun parse-setup (setup-input)
  (loop with lines = (str:lines setup-input)
          ;; The last character of the setup instructions is the total number
          ;; of stacks.
        with number-of-stacks =
          (digit-char-p (uiop:last-char setup-input))
          ;; Initialize the stacks as a vector of vectors.
        with stacks =
          (make-array number-of-stacks
            :adjustable t
            :initial-contents
            (loop repeat number-of-stacks
                  collect (make-array 0 :adjustable t :fill-pointer 0)))
        for line in (butlast lines)
          ;; Loop over the characters in the line, collecting only the actual
          ;; characters and adding it to the correct stack.
        do (loop for i from 0
                 for char across line
                   when (and (oddp i) (not (str:blankp char)))
                 do (vector-push-extend char (elt stacks (floor i 4))))
        finally (return (map 'vector #'nreverse stacks))))

(defun parse-instructions (instructions-input)
  (loop for line in (str:lines instructions-input)
        for (move from to) =
          (mapcar #'parse-integer
              (cl-ppcre:all-matches-as-strings "\\d+" line))
        collect (list :move move :from from :to to)))

(defun apply-instruction (stacks instruction)
  (destructuring-bind (&key move from to) instruction
    (loop repeat move
          for crate = (vector-pop (elt stacks (- from 1)))
          do (vector-push-extend crate (elt stacks (- to 1)))
          finally (return stacks))))

(defun read-top-crates (stacks)
  (format nil "~{~A~}"
    (loop for stack across stacks
          for top-crate = (elt stack (- (length stack) 1))
            collecting top-crate)))

(defun solve ()
  (multiple-value-bind
      (stacks instructions)
      (parse (str:from-file "./input/05.txt"))
    (loop for instruction in instructions
          do (apply-instruction stacks instruction)
          finally (return stacks))
    (read-top-crates stacks)))
