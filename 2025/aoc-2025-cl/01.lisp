;; Useful Slime command: C-c ~
(in-package :aoc-2025-cl)

;; Take a look at this
;; https://lispcookbook.github.io/cl-cookbook/emacs-ide.html

(deftype instruction ()
  '(sr:tuple (member :left :right) integer))

(sr:-> parse-line (string) instruction)
(defun parse-line (line)
  (let ((dir (tr:match (char line 0)
               (#\L :left)
               (#\R :right)))
        (amount (parse-integer line :start 1)))
    (list dir amount)))

(parse-line "L123")

(defun parse-input ()
  (sr:~>> "input/01.test"
    ax:read-file-into-string
    sr:lines
    (mapcar #'parse-line)))

(loop for instruction in (parse-input)
      do (format t "~A~%" instruction))
