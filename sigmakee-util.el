(defun join (separator list)
  "Same as Perl join"
  (setq value "")
  (let* ((first nil)
	 (value
	  (dolist (elt list value)
	    (setq value (concat value (if first separator "") elt))
	    (setq first t))))
    value))

(provide 'sigmakee-util)
