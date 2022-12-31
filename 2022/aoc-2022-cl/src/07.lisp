(defpackage day-07a
  (:use :cl :alexandria :serapeum))
(in-package :day-07a)

;;; File system

(defclass file ()
    ((name
       :initarg :name
       :accessor name)
     (size
       :initarg :size
       :initform 0
       :accessor size)
     (type
      :initarg :type
      :initform :file)
     (children
       :initform (make-hash-table :test 'equal)
       :accessor children)))

(defun make-file (name size)
  (make-instance 'file :name name :type :file :size size))

(defun make-dir (name)
  (make-instance 'file :name name :type :dir))

(defmethod is-a ((file file) type)
  (eq type (slot-value file 'type)))

(defmethod add-child ((file file) (child file))
  (setf (href (children file) (name child)) child))

(defmethod get-child ((file file) name)
  (href (children file) name))

(defmethod iterate ((file file) fn &optional (depth 0))
  (funcall fn file depth)
  (loop for child being the hash-value in (children file)
        do (iterate child fn (1+ depth))))

(defmethod pretty-print ((file file))
  (iterate file
           (lambda (f depth)
             (format t "~v@{~A~:*~}└" depth " ")
             (if (is-a f :file)
                 (format t "~A ~A~%" (name f) (size f))
                 (format t "~A~%" (name f))))))

(defmethod total-size ((file file) &optional (acc 0))
  (if (is-a file :file)
      (+ acc (size file))
      (loop for child being the hash-value in (children file)
              summing (total-size child acc))))

;;; Terminal session

(defclass terminal-session ()
    ((dirstack
       :initform '()
       :accessor dirstack)
     (filesystem
       :initform (make-dir "/")
       :accessor filesystem)))

(defun make-terminal-session ()
  (make-instance 'terminal-session))

(defmethod getcwd ((ts terminal-session))
  (loop with cwd = (filesystem ts)
        for dir in (reverse (dirstack ts))
        do (setf cwd (get-child cwd dir))
        finally (return cwd)))

(defmethod mkdir ((ts terminal-session) name)
  (add-child (getcwd ts) (make-dir name)))

(defmethod touch ((ts terminal-session) name size)
  (add-child (getcwd ts) (make-file name size)))

(defmethod cd ((ts terminal-session) name)
  (if (string= name "..")
      (pop (dirstack ts))
      (push name (dirstack ts))))

;;; Parsing

(defun parse-line (line)
  (progn
   (cl-ppcre:register-groups-bind (dir) ("\\$ cd ([a-z\\.]+)" line)
     (when dir
           (return-from parse-line (list :cd dir))))
   (cl-ppcre:register-groups-bind (dir) ("^dir (\\w+)" line)
     (when dir
           (return-from parse-line (list :mkdir dir))))
   (cl-ppcre:register-groups-bind (size file) ("(\\d+) ([a-z\\.]+)" line)
     (when (and size file)
           (return-from parse-line (list :touch file (parse-integer size)))))))

(defun parse-input (input)
  (with-open-file (stream input)
    (loop with ts = (make-terminal-session)
          for line = (read-line stream nil)
          while line
          for action = (parse-line line)
          do (trivia:match action
               ((list :cd dir) (cd ts dir))
               ((list :mkdir dir) (mkdir ts dir))
               ((list :touch file size) (touch ts file size)))
          finally (return (filesystem ts)))))

;; Solving

(defun dirsizes (fs)
  (let ((sizes '()))
    (iterate fs
             (lambda (file depth)
               (declare (ignore depth))
               (when (is-a file :dir)
                     (push (total-size file) sizes))))
    sizes))

(defun solve-a (dirsizes)
  (loop for size in dirsizes
          when (<= size 100000)
          sum size))

(defun solve-b (total-used-space dirsizes)
  (loop with total-disk-space = 70000000
        with needed-space = 30000000
        with total-unused-space = (- total-disk-space total-used-space)
        with min-delete-size = (- needed-space total-unused-space)
        for size in dirsizes
          when (>= size min-delete-size)
          minimize size into min-size
        finally (return min-size)))

(defun solve ()
  (let* ((fs (parse-input "./input/07.txt"))
         (dirsizes (dirsizes fs))
         (total-used-space (total-size fs)))
    (format t "Part A: ~A~%" (solve-a dirsizes))
    (format t "Part B: ~A~%" (solve-b total-used-space dirsizes))))
