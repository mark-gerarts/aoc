(defun get-side-length (circle-nr)
  (1+ (* 2 circle-nr)))

(defun get-side-circum (circle-nr)
  (if (= circle-nr 0)
      1
      (* circle-nr 8)))

(defun get-start-nr (circle-nr)
  (1+ (loop for i from 0 to (1- circle-nr)
            sum (get-side-circum i))))

(defun get-middles (circle-nr)
  (let* ((start-nr (get-start-nr circle-nr))
         (first (+ start-nr (1- circle-nr)))
         (distance (/ (get-side-circum circle-nr) 4)))
    (loop for i from 0 to 3
          collect (+ first (* i distance)))))

(defun get-distance-to-middle (x xs)
  (apply 'min (mapcar #'(lambda (n) (abs (- n x))) xs)))

(defun get-circle-nr (pos)
  (loop for i from 0
        when (> (get-start-nr i) pos) return (1- i)))

(defun get-shortest-distance (n)
  (let* ((circle-nr (get-circle-nr n))
         (middles (get-middles circle-nr)))
    (+ circle-nr (get-distance-to-middle n middles))))
