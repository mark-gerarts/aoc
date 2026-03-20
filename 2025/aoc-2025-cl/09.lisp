(in-package :aoc-2025-cl)

(defun parse-point (raw-point)
  (mapcar #'parse-integer (sr:split-sequence #\, raw-point)))

(defun parse-input ()
  (sr:~>
   (uiop:read-file-lines "input/09.txt")
   (remove-if #'uiop:emptyp _)
   (mapcar #'parse-point _)))

(defun area (p1 p2)
  (tr:match (list p1 p2)
    ((list (list x1 y1) (list x2 y2))
     (* (1+ (abs (- x2 x1))) (1+ (abs (- y2 y1)))))))

(defun line-segments (points)
  (cons (list (ax:last-elt points) (first points))
        (mapcar #'list points (cdr points))))

(defun rectangles-sorted-by-area (points)
  (let ((rectangles
          (loop for point in points
                nconc (loop for other-point in points
                            for area = (area point other-point)
                            unless (equalp point other-point)
                              collect (list point other-point area)))))
    (sort rectangles (sr:op (> (ax:lastcar _) (ax:lastcar _))))))

(defun part-1 (sorted-rects)
  (ax:lastcar (first sorted-rects)))

(defun sort-point-pair (p1 p2)
  (tr:match (list p1 p2)
    ((list (list x1 y1) (list x2 y2))
     (list (list (min x1 x2) (min y1 y2))
           (list (max x1 x2) (max y1 y2))))))

(defun rectangle-contained-p (segments p1 p2)
  (loop for segment in segments
        for ((p q) (r s)) = (apply #'sort-point-pair segment)
        for ((x y) (u v)) = (sort-point-pair p1 p2)
        when (and (< p u) (< q v) (> r x) (> s y))
          do (return-from rectangle-contained-p nil))
  t)

(defun part-2 (sorted-rects segments)
  (loop for rect-with-area in sorted-rects
        for (p1 p2 area) = rect-with-area
        when (rectangle-contained-p segments p1 p2)
          do (return area)))

(let* ((points (parse-input))
       (sorted-rects (rectangles-sorted-by-area points))
       (line-segments (line-segments points)))
  (format t "Part 1: ~A~%" (part-1 sorted-rects))
  (format t "Part 2: ~A~%" (part-2 sorted-rects line-segments)))
