(add-to-list 'auto-mode-alist '("\\.kif\\'" . sigmakee-mode))

(load "/var/lib/myfrdcsa/codebases/minor/sigmakee-mode/sigmakee-terms.el")
(load "/var/lib/myfrdcsa/codebases/minor/sigmakee-mode/sigmakee-fontify.el")

(define-derived-mode sigmakee-mode
 emacs-lisp-mode "SigmaKEE"
 "Major mode for Sigma Knowledge Engineering Environment.
\\{sigmakee-mode-map}"
 (setq case-fold-search nil)
 (define-key sigmakee-mode-map [tab] 'sigmakee-mode-complete)
 ;; (suppress-keymap sigmakee-mode-map)

 (make-local-variable 'font-lock-defaults)
 (setq font-lock-defaults '(sigmakee-font-lock-keywords nil nil))
 (re-font-lock)
 )

(defun sigmakee-mode-complete (&optional predicate)
 "Perform completion on SigmaKEE symbol preceding point.
Compare that symbol against the known SigmaKEE symbols.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
this-command-keyssymbols with function definitions, values or properties are
considered."
 (interactive)
 (let* ((end (point))
	(beg (with-syntax-table emacs-lisp-mode-syntax-table
	      (save-excursion
	       (backward-sexp 1)
	       (while (or
		       (= (char-syntax (following-char)) ?\')
		       (char-equal (following-char) ?\$)
		       (char-equal (following-char) ?\#))
		(forward-char 1))
	       (point))))
	(pattern (buffer-substring-no-properties beg end))
	;; (sigmakee-output
	;;  (sigmakee-query
	;;   (concat "(constant-complete " "\"" pattern "\")\n")))
	;; (completions
	;;  (cm-convert-string-to-alist-of-strings
	;;   (progn
	;;    (string-match "(\\([^\)]*\\))" ; get this from Cyc and format it into an alist
	;;     cm-cyc-output)
	;;    (match-string 1 cm-cyc-output))))
	(completions
	 sigmakee-mode-all-terms)
	(completion (try-completion pattern completions)))
  (cond ((eq completion t))
   ((null completion)
    (message "Can't find completion for \"%s\"" pattern)
    (ding))
   ((not (string= pattern completion))
    (delete-region beg end)
    (insert completion))
   (t
    ;; (message "Making completion list...")
    ;; (let ((list (all-completions pattern completions)))
    ;;  (setq list (sort list 'string<))
    ;;  (with-output-to-temp-buffer "*Completions*"
    ;;   (display-completion-list list)))
    ;; (message "Making completion list...%s" "done")
    (let* 
     ((expansion (completing-read "Term: " 
		  (all-completions pattern completions) nil nil pattern))
      (regex (concat pattern "\\(.+\\)")))

     (string-match regex expansion)
     (insert (match-string 1 expansion)))))))
