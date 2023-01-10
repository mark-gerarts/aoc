(defun get-rows (input)
  "Parses the string input into a list of integer lists."
  (loop for row in (str:lines input)
        collect (mapcar #'parse-integer (str:words row))))

(defun even-div-p (a b)
  (cond ((= a b) nil)
        ((integerp (/ a b)) (/ a b))
        ((integerp (/ b a)) (/ b a))))

(defun get-line-diff (row)
  (loop for i in row
        when (loop for j in row when (even-div-p i j) return it)
        return it))

(defun checksum (spreadsheet)
  (loop for row in (get-rows spreadsheet)
        sum (get-line-diff row)))
