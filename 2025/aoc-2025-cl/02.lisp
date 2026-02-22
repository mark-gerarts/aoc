(in-package :aoc-2025-cl)

(setf lparallel:*kernel* (lparallel:make-kernel 8))

(defun parse-range (raw-range)
  (tr:match (uiop:split-string raw-range :separator "-")
    ((list start end) (cons (parse-integer start) (parse-integer end)))
    (_ (error "Invalid range"))))

(defun parse-input ()
  (sr:as~> "input/02.txt" <>
           (uiop:read-file-string <>)
           (sr:trim-whitespace <>)
           (uiop:split-string <> :separator ",")
           (mapcar #'parse-range <>)))

(defun is-repeated-twice (sequence)
  (multiple-value-bind (left right) (sr:halves sequence)
    (string= left right)))

(defun is-repeated-at-least-twice (sequence)
  (loop for batch-size from (floor (length sequence) 2) downto 1
        when (apply #'sr:equal* (sr:batches sequence batch-size))
          return t))

(defun sum-invalid-ids (range is-invalid-p)
  (loop with (range-start . range-end) = range
        for id from range-start to range-end
        when (funcall is-invalid-p (write-to-string id))
          sum id))

(defun sum-all-invalid-ids (is-invalid-p)
  (sr:~>> (parse-input)
          (lparallel:pmap
           'list
           (lambda (range) (sum-invalid-ids range is-invalid-p)))
          (reduce #'+)))

(format t "Part 1: ~A~%" (sum-all-invalid-ids #'is-repeated-twice))
(format t "Part 1: ~A~%" (sum-all-invalid-ids #'is-repeated-at-least-twice))
