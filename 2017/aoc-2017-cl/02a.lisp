(defun get-rows (input)
  "Parses the string input into a list of integer lists."
  (loop for row in (str:lines input)
        collect (mapcar #'parse-integer (str:words row))))

(defun get-line-diff (row)
  (let ((smallest (apply 'min row))
        (biggest (apply 'max row)))
    (- biggest smallest)))

(defun checksum (spreadsheet)
  (loop for row in (get-rows spreadsheet)
        sum (get-line-diff row)))
