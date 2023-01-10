(defstruct square
  pos
  value)

(defun init-grid ()
  "Creates the initial state of the grid. We have to prefill it with 2 squares
   to be able to start generating the next ones."
  (let ((grid (make-array 0
                          :fill-pointer 0
                          :adjustable t
                          :element-type 'square))
        (first-square (make-square :pos (cons 0 0) :value 1))
        (snd-square (make-square :pos (cons 1 0) :value 1)))
    (vector-push-extend first-square grid)
    (vector-push-extend snd-square grid)
    grid))

(defun get-neighbour-coords (pos)
  (let ((x (car pos))
        (y (cdr pos)))
    (list (cons (1- x) y)
          (cons (1- x) (1- y))
          (cons (1- x) (1+ y))
          (cons x (1- y))
          (cons x (1+ y))
          (cons (1+ x) y)
          (cons (1+ x) (1- y))
          (cons (1+ x) (1+ y)))))

(defun has-neighbour (square dir grid)
  (let* ((pos (square-pos square))
         (x (car pos))
         (y (cdr pos))
         (target-pos (case dir
                       (:left (cons (1- x) y))
                       (:right (cons (1+ x) y))
                       (:up (cons x (1+ y)))
                       (:down (cons x (1- y))))))
    (get-square-at-pos target-pos grid)))

(defun get-square-at-pos (pos grid)
  (find-if #'(lambda (sq) (equal (square-pos sq) pos)) grid))

(defun get-neighbours (square grid)
  (let ((coords (get-neighbour-coords (square-pos square))))
    (loop for coord in coords
          when (get-square-at-pos coord grid) collect it)))

(defun get-last-square (grid)
  "Retrieves the last square that was generated."
  (elt grid (1- (length grid))))

(defun get-next-pos (grid)
  "Determines the next position of the spiral."
  (let* ((last-square (get-last-square grid))
         (last-square-pos (square-pos last-square))
         (x (car last-square-pos))
         (y (cdr last-square-pos)))
    (flet ((has (dir) (has-neighbour last-square dir grid)))
      (if (has :left)
          (if (not (has :up))
              (cons x (1+ y))           ; Up
              (cons (1+ x) y))          ; Right
          (if (has :down)
              (cons (1- x) y)           ; Left
              (if (has :right)
                  (cons x (1- y))       ; Down
                  (cons (1+ x) y))))))) ; Left

(defun generate-next-square (grid)
  "Generates the next square. This means: getting the next position and
   calculating the value."
  (let* ((next-pos (get-next-pos grid))
         (new-square (make-square :pos next-pos))
         (new-value (get-value new-square grid)))
    (setf (square-value new-square) new-value)
    (vector-push-extend new-square grid)))

(defun get-value (square grid)
  "Calculates the value this square should have by summing the nieghbouring
   values."
  (let ((neighbours (get-neighbours square grid)))
    (apply '+ (mapcar #'square-value neighbours))))

(defun get-last-value (grid)
  (square-value (get-last-square grid)))

(defun get-value-higher-than (number)
  "Main function to solve the input."
  (let ((grid (init-grid)))
    (loop while (>= number (get-last-value grid))
          do (generate-next-square grid))
    (get-last-value grid)))
