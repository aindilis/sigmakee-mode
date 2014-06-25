(defun sigmakee-mode-load-assertion-into-stack ()
 ""
 (interactive)
 (let*
  ((item freekbs2-stack)
   (original-string (substring-no-properties (thing-at-point 'sexp)))
   (dequestionmarked-string 
    (replace-regexp-in-string "\\(\\?[A-Z0-9-_]+\\)\\b"
     (lambda (match) 
      (concat "var-" (progn
		      (string-match "^\?\\(.+\\)$" match)
		      (match-string 1 match))))
     original-string t))
   (read-dequestionmarked-string (read dequestionmarked-string))
   (freekbs2ified-read-dequestionmarked-string
    (sigmakee-convert-to-freekbs2-format read-dequestionmarked-string))
   )
  (setq freekbs2-stack freekbs2ified-read-dequestionmarked-string)
  (freekbs2-ring-add-result item)))

(defun sigmakee-convert-from-read-dequestionmarked-string-to-freekbs2-list (formula)
 "Convert from emacs symbols and strings to strings and strings
containing strings (compatible with FreeKBS)"
 (interactive)
 (cond
  ((listp formula)
   (mapcar (lambda (subformula)
	    (sigmakee-convert-from-read-dequestionmarked-string-to-freekbs2-list
	     subformula)) formula))
  ((stringp formula)
   (prin1-to-string formula))
  ((symbolp formula)
   (cond
    ((non-nil (string-match "^var-[A-Z0-9-_]+$" (prin1-to-string formula)))
     formula)
    (t (prin1-to-string formula))))))

(defun sigmakee-convert-from-freekbs2-list-to-read-dequestionmarked-string (formula)
 "Convert from emacs strings and strings containing
strings (compatible with FreeKBS) to emacs symbols and strings"
 (interactive)
 (cond
  ((listp formula)
   (mapcar (lambda (subformula)
 	    (sigmakee-convert-from-freekbs2-list-to-read-dequestionmarked-string subformula)) 
    formula))
  ((stringp formula)
   (read formula))
  ((symbolp formula)
   formula)))

(defun sigmakee-convert-from-read-dequestionedmarked-string-to-original-string (formula)
 "Convert from emacs strings and strings containing
strings (compatible with FreeKBS) to emacs symbols and strings"
 (interactive)
 ;; (see "formula")
 ;; (see formula)
 (cond
  ((listp formula)
   (concat "(" 
    (join " "
     (mapcar (lambda (subformula)
	      (sigmakee-convert-from-read-dequestionedmarked-string-to-original-string
		     subformula)
	      ;; (see subformula)
	      ;; (see (concat "hey: " 
	      ;; 	    ))
	      )
      formula)) ")"))
  ((stringp formula)
   (prin1-to-string formula))
  ((symbolp formula)
   (replace-regexp-in-string "^\\(var-[A-Z0-9-_]+\\)$" 	 
    (lambda (match) 
     (concat "?" (progn
		  (string-match "^var-\\(.+\\)$" match)
		  (match-string 1 match))))
    (prin1-to-string formula)))))

(defun sigmakee-mode-print-assertion-from-stack ()
 ""
 (interactive)
 (let* ((read-dequestionmarked-string
	 (sigmakee-convert-from-freekbs2-list-to-read-dequestionmarked-string
	  freekbs2-stack))
	(original-string 
	 (progn (see read-dequestionmarked-string)
	  (sigmakee-convert-from-read-dequestionedmarked-string-to-original-string
	  read-dequestionmarked-string))))
  (see original-string)))

(provide 'sigmakee-freekbs2)

;; (sigmakee-mode-print-assertion-from-stack)

;; (sigmakee-convert-from-read-dequestionedmarked-string-to-original-string 
;;  (read "(kif test var-X)"))
