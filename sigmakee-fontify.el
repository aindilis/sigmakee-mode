;;; sigmakee-fontify.el --- a part of the simple SigmaKEE mode

(defgroup sigmakee-faces nil "faces used for SigmaKEE mode"  :group 'faces)

(defvar in-xemacs-p "" nil)

;;; GNU requires that the face vars be defined and point to themselves

(defvar sigmakee-main-keyword-face 'sigmakee-main-keyword-face
  "Face to use for SigmaKEE relations.")
(defface sigmakee-main-keyword-face
  '((((class color)) (:foreground "red" :bold t)))
  "Font Lock mode face used to highlight class refs."
  :group 'sigmakee-faces)

(defvar sigmakee-function-nri-and-class-face 'sigmakee-function-nri-and-class-face
  "Face to use for SigmaKEE keywords.")
(defface sigmakee-function-nri-and-class-face
    (if in-xemacs-p 
	'((((class color)) (:foreground "red"))
	  (t (:foreground "gray" :bold t)))
      ;; in GNU, no bold, so just use color
      '((((class color))(:foreground "red"))))
  "Font Lock mode face used to highlight property names."
  :group 'sigmakee-faces)

(defvar sigmakee-normal-face 'sigmakee-normal-face "regular face")
(defface sigmakee-normal-face
 '((t (:foreground "grey")))
 "Font Lock mode face used to highlight property names."
 :group 'sigmakee-faces)

(defvar sigmakee-string-face 'sigmakee-string-face "string face")
(defface sigmakee-string-face
    '((t (:foreground "green4")))
  "Font Lock mode face used to highlight strings."
  :group 'sigmakee-faces)

(defvar sigmakee-logical-operator-face 'sigmakee-logical-operator-face
  "Face to use for SigmaKEE logical operators (and, or, not, exists, forall, =>, <=>)")
;; same as function name face
(defface sigmakee-logical-operator-face
 '((((class color)) (:foreground "blue")))
  "Font Lock mode face used to highlight class names in class definitions."
  :group 'sigmakee-faces)

(defvar sigmakee-main-relation-face 'sigmakee-main-relation-face
  "Face to use for SigmaKEE relations.")
(defface sigmakee-main-relation-face
  '((((class color)) (:foreground "black" :bold t)))
  "Font Lock mode face used to highlight class refs."
  :group 'sigmakee-faces)

(defvar sigmakee-relation-face 'sigmakee-relation-face
  "Face to use for SigmaKEE relations.")
(defface sigmakee-relation-face
  '((((class color)) (:foreground "darkgrey")))
  "Font Lock mode face used to highlight class refs."
  :group 'sigmakee-faces)

;; (defvar sigmakee-property-face 'sigmakee-property-face
;;   "Face to use for SigmaKEE property names in property definitions.")
;; (defface sigmakee-property-face
;;   (if in-xemacs-p  
;;      '((((class color)) (:foreground "darkviolet" :bold t))
;;        (t (:italic t)))
;;     ;; in gnu, just magenta
;;     '((((class color)) (:foreground "darkviolet"))))
;;      "Font Lock mode face used to highlight property names."
;;      :group 'sigmakee-faces)

(defvar sigmakee-variable-face 'sigmakee-variable-face
  "Face to use for SigmaKEE property name references.")
(defface sigmakee-variable-face
  '((((class color)) (:foreground "darkviolet" ))
    (t (:italic t)))
  "Font Lock mode face used to highlight property refs."
  :group 'sigmakee-faces)

(defvar sigmakee-comment-face 'sigmakee-comment-face
  "Face to use for SigmaKEE comments.")
(defface sigmakee-comment-face
  '((((class color) ) (:foreground "red" :italic t))
    (t (:foreground "DimGray" :italic t)))
  "Font Lock mode face used to highlight comments."
  :group 'sigmakee-faces)

(defvar sigmakee-other-face 'sigmakee-other-face
  "Face to use for other keywords.")
(defface sigmakee-other-face
  '((((class color)) (:foreground "peru")))
  "Font Lock mode face used to highlight other SigmaKEE keyword."
  :group 'sigmakee-faces)

;; (defvar sigmakee-tag-face 'sigmakee-tag-face
;;   "Face to use for tags.")
;; (defface sigmakee-tag-face
;;     '((((class color)) (:foreground "violetred" ))
;;       (t (:foreground "black")))
;;   "Font Lock mode face used to highlight other untyped tags."
;;   :group 'sigmakee-faces)

;; (defvar sigmakee-substitution-face 'sigmakee-substitution-face "face to use for substitution strings")
;; (defface sigmakee-substitution-face
;;     '((((class color)) (:foreground "orangered"))
;;       (t (:foreground "lightgrey")))
;;   "Face to use for SigmaKEE substitutions"
;;   :group 'sigmakee-faces)


;;;================================================================
;;; these are the regexp matches for highlighting SigmaKEE 

(defvar sigmakee-font-lock-prefix "\\b")
(defvar sigmakee-font-lock-keywords
  (let ()
    (list 

     ;; (list
     ;;  "^[^;]*\\(;.*\\)$" '(1 sigmakee-comment-face nil))

     (list 
      ;; (concat "^\s*[^;][^\n\r]*[\s\n\r(]\\b\\(and\\|or\\|not\\|exists\\|forall\\)\\b"
      (concat "\\b\\(and\\|or\\|not\\|exists\\|forall\\)\\b"
	      )
      '(1 sigmakee-logical-operator-face nil)
      )
     
     (list 
      (concat sigmakee-font-lock-prefix "\\(" (join "\\|"
	      sigmakee-mode-main-relation ) "\\)\\b" ) '(1
	      sigmakee-main-relation-face nil) )

     (list
      (concat sigmakee-font-lock-prefix "\\(" 
       (join "\\|"
	sigmakee-mode-functions-non-relational-instances-and-classes) "\\)\\b")
      '(1 sigmakee-function-nri-and-class-face nil))

     (list 
      (concat
       sigmakee-font-lock-prefix "\\([_a-zA-Z0-9-]+Fn\\)\\b" )
       '(1 sigmakee-function-nri-and-class-face nil) )

     (list 
      (concat "\\(\\?[_A-Za-z0-9-]+\\)\\b"
	      )
      '(1 sigmakee-variable-face nil)
      )

     (list 
      (concat "\\(\\&\\%[_A-Za-z0-9-]+\\)\\b"
	      )
      '(1 sigmakee-other-face nil)
      )

     (list 
      (concat sigmakee-font-lock-prefix "\\(" (join "\\|"
     	      sigmakee-mode-relations) "\\)\\b" ) '(1
     	      sigmakee-relation-face nil) )

     (list 
      ;; (concat "^\s*[^;][^\n\r]*[\s\n\r(]\\(=>\\|<=>\\)"
      (concat "\\(=>\\|<=>\\)")
      '(1 sigmakee-logical-operator-face nil)
      )

     (list 
      (concat sigmakee-font-lock-prefix "\\(" (join "\\|"
	      sigmakee-mode-main-keyword ) "\\)\\b" ) '(1
	      sigmakee-main-keyword-face nil) )
     
     ;; black for the def parts of PROPERTY DEFINITION
     ;; and of TransitiveProperty UnambiguousProperty UniqueProperty
;;; END OF LIST ELTS
     ))

    "Additional expressions to highlight in SigmaKEE mode.")



(put 'sigmakee-mode 'font-lock-defaults '(sigmakee-font-lock-keywords nil nil))

(defun re-font-lock () (interactive) (font-lock-mode 0) (font-lock-mode 1))

(provide 'sigmakee-fontify)
