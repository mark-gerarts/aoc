(in-package :aoc-2025-cl)

(defstruct pos (x 0 :type fixnum) (y 0 :type fixnum) (z 0 :type fixnum))

(defmethod distance ((a pos) (b pos))
  (+ (expt (- (pos-x a) (pos-x b)) 2)
     (expt (- (pos-y a) (pos-y b)) 2)
     (expt (- (pos-z a) (pos-z b)) 2)))

(defun parse-pos (raw-pos)
  (tr:match (mapcar #'parse-integer (sr:split-sequence #\, raw-pos))
    ((list x y z) (make-pos :x x :y y :z z))
    (_ (error "Invalid range"))))

(defun parse-input ()
  (sr:~>> "input/08.txt"
          uiop:read-file-lines
          (remove-if #'uiop:emptyp)
          (mapcar #'parse-pos)))

(defun sort-by-distance (positions)
  (sr:~>
   (loop for index from 1
         for pos in positions
         nconc (loop for other-index from index below (length positions)
                     for other-pos = (nth other-index positions)
                     collect (list (distance pos other-pos) pos other-pos)))
   (sort (sr:op (< (first _) (first _))))
   (mapcar #'cdr _)))

(defun find-box-circuit-index (all-circuits box)
  (position-if (sr:op (member box _ :test #'equalp)) all-circuits))

(defun connect-circuits (all-circuits a b)
  (let ((circuit-a-index (find-box-circuit-index all-circuits a))
        (circuit-b-index (find-box-circuit-index all-circuits b)))
    (cons (union (nth circuit-a-index all-circuits)
                 (nth circuit-b-index all-circuits))
          (loop for i from 0
                for circuit in all-circuits
                unless (or (= circuit-a-index i) (= circuit-b-index i))
                  collect circuit))))

(defun connect-closest (boxes sorted-input max-steps)
  (loop with circuits = (mapcar #'list boxes)
        for i from 0
        for pair in sorted-input
        for (a b) = pair
        do (setf circuits (connect-circuits circuits a b))
        when (or (= 1 (length circuits)) (= max-steps i))
          do (return (values circuits a b))))

(defun part-1 (boxes sorted-input)
  (sr:~>
   (connect-closest boxes sorted-input 1000)
   (mapcar #'length _)
   (sort #'>)
   (sr:take 3 _)
   (apply #'* _)))

(defun part-2 (boxes sorted-input)
  (multiple-value-bind
        (_ a b)
      (connect-closest boxes sorted-input (length sorted-input))
    (declare (ignore _))(* (pos-x a) (pos-x b))))

(let* ((boxes (parse-input))
       (sorted-input (sort-by-distance boxes)))
  (format t "Part 1: ~A~%" (part-1 boxes sorted-input))
  (format t "Part 2: ~A~%" (part-2 boxes sorted-input)))
