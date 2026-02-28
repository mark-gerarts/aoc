(in-package :aoc-2025-cl)

(defun words (string)
  "Like serapeum:words, but considers everything non-space a word"
  (sr:split-sequence #\Space string :remove-empty-subseqs t))

(defun transpose (sequences)
  (loop for col from 0 below (length (first sequences))
        collect (loop for seq in sequences
                      collect (elt seq col))))

(defun number-parser-part-1 (raw-number-lines)
  (sr:~>> raw-number-lines
          (mapcar (sr:op (mapcar #'parse-integer (words _))))
          transpose))

(defun number-parser-part-2 (raw-number-lines)
  (sr:~>> raw-number-lines
          transpose
          (mapcar (sr:op (coerce _ 'string)))
          (mapcar (sr:op (remove #\Space _)))
          (sr:split-sequence "" _ :test #'equal)
          (mapcar (sr:op (mapcar #'parse-integer _)))))

(defun parse-input (number-parser)
  (let* ((input-lines
           (remove "" (uiop:read-file-lines "input/06.txt") :test #'equal))
         (numbers (funcall number-parser (butlast input-lines)))
         (operations (mapcar #'intern (words (ax:last-elt input-lines)))))
    (list :numbers numbers :operations operations)))

(defun solve (input)
  (reduce #'+
          (mapcar (lambda (col op) (reduce op col))
                  (getf input :numbers)
                  (getf input :operations))))

(format t "Part 1: ~A~%" (solve (parse-input #'number-parser-part-1)))
(format t "Part 2: ~A~%" (solve (parse-input #'number-parser-part-2)))
