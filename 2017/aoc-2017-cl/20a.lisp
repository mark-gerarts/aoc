(use-package 'cl-ppcre)

(defstruct vect x y z)
(defstruct particle position velocity acceleration)

(defun parse-vector (vector-string)
  (let* ((coords (mapcar #'parse-integer (str:split "," vector-string)))
         (x (nth 0 coords))
         (y (nth 1 coords))
         (z (nth 2 coords)))
    (make-vect :x x :y y :z z)))

(defun parse-line (line)
  (let* ((pos (parse-vector (scan-to-strings "(?<=p=<).*(?=>, v)" line)))
         (vel (parse-vector (scan-to-strings "(?<=v=<).*(?=>, a)" line)))
         (acc (parse-vector (scan-to-strings "(?<=a=<).*(?=>)" line))))
    (make-particle :position pos :velocity vel :acceleration acc)))

(defun parse-input (input)
  (loop for line in (str:lines input) collect (parse-line line)))

(defun distance-to-origin (vector)
  (+ (abs (vect-x vector)) (abs (vect-y vector)) (abs (vect-z vector))))

(defun add (v1 v2)
  (make-vect :x (+ (vect-x v1) (vect-x v2))
             :y (+ (vect-y v1) (vect-y v2))
             :z (+ (vect-z v1) (vect-z v2))))

(defun step-particle (particle)
  (let ((new-velocity (add (particle-velocity particle)
                           (particle-acceleration particle))))
  (make-particle
   :position (add (particle-position particle) new-velocity)
   :velocity new-velocity
   :acceleration (particle-acceleration particle))))

(defun step-all-particles (particle-list)
  (mapcar #'step-particle particle-list))

(defun get-closest-to-origin (particle-list)
  (let* ((distances (mapcar
                     (lambda (p) (distance-to-origin (particle-position p)))
                     particle-list))
         (closest (apply #'min distances)))
    (position closest distances)))

(defun solve-with-n-steps (n input)
  (let ((particles (parse-input input)))
    (loop for i from 0 to n do (setf particles (step-all-particles particles)))
    (get-closest-to-origin particles)))
