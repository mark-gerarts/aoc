(in-package :aoc-2025-cl)

(defun to-digits (bank)
  (map 'list #'digit-char-p bank))

(defun to-number (digits)
  (reduce (lambda (acc d) (+ (* acc 10) d)) digits))

(defun get-maximal-sublist (digits min-length)
  (labels
      ((go-rec (digits current-largest)
         (cond
           ((< (length (cdr digits)) min-length) current-largest)
           ((< (car current-largest) (cadr digits))
            (go-rec (cdr digits) (cdr digits)))
           (t (go-rec (cdr digits) current-largest)))))
    (go-rec digits digits)))

(defun largest-joltage (digits num-batteries)
  (labels
      ((go-rec (digits num-batteries)
         (if (= 0 num-batteries)
             '()
             (destructuring-bind
                 (digit . rest)
                 (get-maximal-sublist digits num-batteries)
               (cons digit (go-rec rest (1- num-batteries)))))))
    (to-number (go-rec digits num-batteries))))

(defun solve (num-batteries)
  (loop for line in (uiop:read-file-lines "input/03.txt")
        for joltage = (largest-joltage (to-digits line) num-batteries)
        sum joltage))

(format t "Part 1: ~A~%Part 2: ~A~%" (solve 2) (solve 12))
