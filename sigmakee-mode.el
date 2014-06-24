(add-to-list 'auto-mode-alist '("\\.kif\\'" . sigmakee-mode))

(load "/var/lib/myfrdcsa/codebases/minor/sigmakee-mode/sigmakee-terms.el")
(load "/var/lib/myfrdcsa/codebases/minor/sigmakee-mode/sigmakee-fontify.el")

(define-derived-mode sigmakee-mode
 emacs-lisp-mode "SigmaKEE"
 "Major mode for Sigma Knowledge Engineering Environment.
\\{sigmakee-mode-map}"
 (setq case-fold-search nil)
 (define-key sigmakee-mode-map "<TAB>" 'sigmakee-mode-complete)
 (suppress-keymap sigmakee-mode-map)

 (make-local-variable 'font-lock-defaults)
 (setq font-lock-defaults '(sigmakee-font-lock-keywords nil nil))
 (re-font-lock)
 )
