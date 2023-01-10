(defun int-to-digits (int)
  (map 'list #'digit-char-p (write-to-string int)))

(defun shift (list n)
  "Shift a list n places to the right."
  (if (equal n 0)
      list
      (shift
       (append (cdr list) (list (car list)))
       (- n (signum n)))))

(defun get-matching-digits (digits)
  (when (not (evenp (length digits))) (error "Input not even"))
  (let ((shifted (shift digits (/ (length digits) 2))))
    (loop for d in digits
          for s in shifted
          when (equal d s) collect d)))

(defun solve-captcha (number)
  (apply '+ (get-matching-digits (int-to-digits number))))
