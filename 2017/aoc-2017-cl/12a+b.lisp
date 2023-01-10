(use 'cl-ppcre)

(defstruct program
  id
  pipes)

(defun parse-line (line)
  "Parses a single line into a program struct."
  (let* ((parts (cl-ppcre:split " <-> " line))
         (id (parse-integer (first parts)))
         (pipe-string (second parts))
         (pipes (mapcar #'parse-integer (cl-ppcre:split ", " pipe-string))))
    (make-program :id id :pipes pipes)))

(defun parse-input (input)
  "Parses the input into a list of programs."
  (loop for line in (str:lines input)
        collect (parse-line line)))

(defun find-by-id (id program-list)
  (find-if #'(lambda (program) (= id (program-id program))) program-list))

(defun get-connected-programs (program program-list)
  (labels ((recur (program-list connected-ids visit-queue)
             (when (null visit-queue)
               (return-from recur connected-ids))
             (let* ((pid (car visit-queue))
                    (connected-ids (cons pid connected-ids))
                    (program (find-by-id pid program-list))
                    (visit-queue (cdr visit-queue))
                    (new-pids (loop for pipe in (program-pipes program)
                                    unless (or (member pipe connected-ids)
                                               (member pipe visit-queue))
                                      collect pipe)))
               (recur program-list
                      connected-ids
                      (append new-pids visit-queue)))))
    (recur program-list '() (list (program-id program)))))

(defun solve-part-a (input)
  (let* ((program-list (parse-input input))
         (target (find-by-id 0 program-list)))
    (length (get-connected-programs target program-list))))

(defun get-all-groups (program-list)
  (let ((groups '()))
    (loop for program in program-list
          do (let ((group (sort
                           (get-connected-programs program program-list)
                           #'<)))
               (unless (member group groups :test #'equal)
                 (setf groups (cons group groups)))))
    groups))

(defun solve-part-b (input)
  (length (get-all-groups (parse-input input))))
