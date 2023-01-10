(use-package :cl-ppcre)

(defstruct disc
  name
  weight
  holding)

(defun parse-line (line)
  "Parses a single input line into a disc struct."
  (let* ((words (str:words line))
         (name (nth 0 words))
         (weight (parse-integer (string-trim "()" (nth 1 words))))
         (holding-split (split " -> " line))
         (holding (if (null (nth 1 holding-split))
                      '()
                      (mapcar #'(lambda (name) (string-trim ", " name))
                              (str:words (nth 1 holding-split))))))
    (make-disc :name name
               :weight weight
               :holding holding)))

(defun parse-input (input)
  "Parses the input into a list of discs."
  (loop for line in (str:lines input)
        collect (parse-line line)))

(defun get-base (discs)
  "When given a flat list of discs, finds out which one is the base.  We'll do
   this by finding the first disk that isn't supported by another one."
  (find-if #'(lambda (disc)
               (not (loop for d in discs
                          when (find (disc-name disc)
                                     (disc-holding d)
                                     :test #'string=)
                            return 't)))
           discs))

(defun find-by-name (name discs)
  (find-if #'(lambda (disc) (string= (disc-name disc) name)) discs))

(defun set-holding-towers (disc discs)
  "Recursive function that replaces the holding tower names with real discs."
  (if (null (disc-holding disc))
      disc
      (make-disc
       :name (disc-name disc)
       :weight (disc-weight disc)
       :holding
       (loop for name in (disc-holding disc)
             collect (set-holding-towers (find-by-name name discs) discs)))))

(defun generate-tower (discs)
  (set-holding-towers (get-base discs) discs))

(defun all-the-same-p (list)
  (every #'(lambda (x) (= x (first list))) list))

(defun average (list)
  (/ (apply '+ list) (length list)))

(defun weight (disc)
  (+ (disc-weight disc)
     (loop for d in (disc-holding disc) sum (weight d))))

(defun get-differing-disc (discs)
  (find-if #'(lambda (disc)
               (= 1 (count disc discs
                           :test #'(lambda (a b)
                                     (= (weight a)
                                        (weight b))))))
           discs))

(defun get-weight-difference (differing-disc discs)
  (- (weight (find-if-not #'(lambda (d) (equal differing-disc d)) discs))
     (weight differing-disc)))

(defun out-of-balance-p (disc)
  (cond ((null (disc-holding disc)) 'nil)
        ((all-the-same-p (mapcar #'weight (disc-holding disc))) 'nil)
        ('t (get-differing-disc (disc-holding disc)))))

(defun find-inbalanced (disc)
  (let ((out-of-balance (out-of-balance-p disc)))
    (if out-of-balance
          (if (find-inbalanced out-of-balance)
              (find-inbalanced out-of-balance)
              disc))
        (loop for d in (disc-holding disc)
              when (find-inbalanced disc) return it)))

(defun get-new-weight (inbalanced-disc)
  (let* ((differing-disc (get-differing-disc (disc-holding inbalanced-disc)))
         (weight-difference
               (get-weight-difference differing-disc
                                      (disc-holding inbalanced-disc))))
     (+ (disc-weight differing-disc) weight-difference)))
