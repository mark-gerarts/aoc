(in-package :aoc-2025-cl)

(defstruct range (start 0 :type fixnum) (end 0 :type fixnum))

(defmethod contains ((range range) id)
  (and (>= id (range-start range)) (<= id (range-end range))))

(defmethod size ((range range))
  (1+ (- (range-end range) (range-start range))))

(defun parse-range (range)
  (sr:~> range
         (uiop:split-string :separator "-")
         (mapcar #'parse-integer _)
         (tr:match ((list start end) (make-range :start start :end end))
           (_ (error "Invalid range")))))

(defun merge-consecutive-ranges (a b)
  (if (<= (range-start b) (range-end a))
      (list (make-range
             :start (range-start a)
             :end (max (range-end a) (range-end b))))
      (list b a)))

(defun merge-ranges (ranges)
  (let ((sorted-ranges (sort ranges #'< :key #'range-start)))
    (reduce
     (lambda (merged-ranges range)
       (nconc
        (merge-consecutive-ranges (car merged-ranges) range)
        (cdr merged-ranges)))
     sorted-ranges
     :initial-value (list (car sorted-ranges)))))

(defun parse-input ()
  (destructuring-bind
      (ranges ids)
      (sr:~> "input/05.txt"
             uiop:read-file-string
             sr:trim-whitespace
             (cl-ppcre:split "\\n\\n" _))
    (values
     (sr:~> ranges
            (uiop:split-string :separator uiop:+LF+)
            (mapcar #'parse-range _)
            merge-ranges)
     (sr:~> ids
            (uiop:split-string :separator uiop:+LF+)
            (mapcar #'parse-integer _)))))

(defun count-fresh-ids (ids fresh-ranges)
  (loop for id in ids
        count (some (sr:op (contains _ id)) fresh-ranges)))

(defun count-all-possible-ids (ranges)
  (loop for range in ranges
        sum (size range)))

(multiple-value-bind (ranges ids) (parse-input)
  (format t "Part 1: ~A~%" (count-fresh-ids ids ranges))
  (format t "Part 2: ~A~%" (count-all-possible-ids ranges)))
