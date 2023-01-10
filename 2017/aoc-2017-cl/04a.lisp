(defun occurences (el list)
  (loop for x in list when (equal el x) count 1))

(defun uniquep (el list)
  (= (occurences el list) 1))

(defun validp (passphrase)
  (let ((words (str:words passphrase)))
    (loop for word in words
          unless (uniquep word words) do (return-from validp 'nil))
    't))

(defun count-valid-passphrases (passphrase-list)
  (loop for passphrase in (str:lines passphrase-list)
        when (validp passphrase) count 1))
