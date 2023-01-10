(defun int-to-digits (int)
  (map 'list #'digit-char-p (write-to-string int)))

(defun get-matching-digits (digits)
  (let ((wrapped (append digits (list (first digits))))
        (shifted (append (last digits) digits)))
    (loop for d1 in shifted
          for d2 in wrapped
          when (equal d1 d2) collect d1)))

(defun solve-captcha (number)
  (apply '+ (get-matching-digits (int-to-digits number))))
