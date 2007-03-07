;;; docomp.el --- byte-compliling stuff for light and ultex
;; Some of the contents of this file were cribbed from docomp.el in
;; William Perry's w3 package.

(setq load-path (cons (expand-file-name "./")
		      load-path))

(setq max-specpdl-size (* 10 max-specpdl-size)
      max-lisp-eval-depth (* 10 max-lisp-eval-depth))

(defun ultex-declare-variables (&rest args)
  (while args
    (eval (list 'defvar (car args) nil ""))
    (setq args (cdr args))))

;; For font-lock:
(ultex-declare-variables 'font-latex-keywords
			 'font-latex-keywords-2
			 'font-lock-defaults-alist)

;; For imenu:
(ultex-declare-variables 'imenu-create-index-function)

;; For bib-cite:
(ultex-declare-variables 'bib-cite-map)

;; For ultra-tex, miscellany
(ultex-declare-variables 'TeX-current-process-region-p
			 'ultra-tex-mode-map
			 ':help-string)

(load "bytecomp" t t nil)
;; Emacs byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars)
      byte-optimize t)

;; convince tex-buf.el that we actually have the AUC-TeX file tex.el,
;; for those users who don't want to have to download the the whole
;; AUC-TeX package.  Similarly for tex-buf, but this to avoid
;; byte-compiling errors.
(provide 'tex)
(provide 'tex-buf)

(if (and (boundp 'emacs-major-version)
	 (= 19 emacs-major-version))
    (setq command-line-args-left
	  (delete "tex-buf.el" command-line-args-left)))    

(defun ultex-byte-compile-quietly ()
  (interactive)
  (setq byte-compile-warnings nil)
  (batch-byte-compile))

;; If we don't have the customization package, define defgroup and
;; defcustom apprpriately.
;; stolen from AUC-TeX:
(condition-case ()
    (require 'custom)
  (error nil))
(if (and (featurep 'custom) (fboundp 'custom-declare-variable))
    nil ;; We've got what we needed
  ;; We have the old custom-library, hack around it!
  (defmacro defgroup (&rest args)
    nil)
  (defmacro defcustom (var value doc &rest args) 
    (` (defvar (, var) (, value) (, doc)))))
