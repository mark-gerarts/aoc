(defun anagramp (a b)
  (equal (sort a 'char<) (sort b 'char<)))

(defun word-valid-p (word other-words)
  (< (loop for w in other-words
        when (anagramp word w) count 1) 2))

(defun validp (passphrase)
  (let ((words (str:words passphrase)))
    (loop for word in words
          unless (word-valid-p word words) do (return-from validp 'nil))
    't))

(defun count-valid-passphrases (passphrase-list)
  (loop for passphrase in (str:lines passphrase-list)
        when (validp passphrase) count 1))
