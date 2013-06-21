(require 'scheme)
;; (require 'quack)
(require 'geiser-mode)
(require 'geiser-repl)
(define-key geiser-repl-mode-map (kbd "M-`") nil)
(define-key geiser-mode-map (kbd "M-`") nil)
(require 'scheme-complete)
(setq geiser-racket-binary "/Applications/Racket v5.3.4/bin/racket")

(defun gambit-setup ()
  "Gambit-C scheme-mode extensions"
  (interactive)
  (setq gambit-highlight-color "gray")
  (if nil (progn
            (require 'gambit "gambit.el" t)
            (defun gambit-abort ()
              (interactive)
              (scheme-send-string ",t")))
    (defun gambit-abort ()
      (interactive)
      (comint-send-string (scheme-proc) ",t\n")))
  (add-hook 'scheme-mode-hook
            (lambda () (local-set-key "\C-cx" 'gambit-abort)))
  (setq scheme-program-name "gsi -:d-")
  (scheme-extend-info "(gambit-c)General Index"))

(defun rc-scheme48 ()
  "Scheme48 scheme-mode extensions"
  (interactive)
  (require 'scheme48)
  (setq scheme-program-name "scheme48")
  (scheme-extend-info "(scheme48)Binding Index"))

(defun rc-guile ()
  "Scheme48 scheme-mode extensions"
  (interactive)
  (setq scheme-program-name "guile"))

(defun rc-mzscheme ()
  "MzScheme scheme-mode setup"
  (interactive)
  (setq scheme-program-name "MzScheme"))

(defun rc-racket ()
  "PLT racket scheme-mode setup"
  (interactive))

(defun rc-chicken ()
  "chicken scheme-mode extensions"
  (interactive)
  (setq scheme-program-name "csi"))

(defun rc-paredit ()
  (require 'paredit)
  (add-hooks '(scheme-mode-hook
               emacs-lisp-mode-hook
               lisp-mode-hook)
             '(lambda () (paredit-mode +1))
             'append))

(defun scheme-add-keywords (keyword-rules)
   (let* ((keyword-list (mapcar #'(lambda (x)
                                    (symbol-name (car x)))
                                keyword-rules))
          (keyword-regexp (concat "(\\("
                                  (regexp-opt keyword-list)
                                  "\\)[ \n]")))
     (font-lock-add-keywords 'scheme-mode
                             `((,keyword-regexp 1 font-lock-keyword-face))))
   (scheme-add-indentations keyword-rules))

(load "scheme-indent")

(defun scheme-add-indentations (rules)
  (mapc #'(lambda (x)
            (put (car x)
                 'scheme-indent-function
                 (cadr x)))
        rules))

(scheme-add-keywords
 '((when 1)
   (unless 1)
   ))

(scheme-add-indentations
 '((and-let* 1)
   (if-let* 1)
   (let-optionals 2)
   (let-optionals* 2)
   (let-port-rest 2)
   (wind-fluid 3)
   (case-equal 1)
   (let-list 1)
   (let-fluid 2)
   (let-sxml-pluck-attrs 2)
   (let-sxml-attrs 2)
   (case-regex 1)
   (case-posix-regex 1)
   (with-form-data 1)
   (let-conversion 1)
   (sxml-convert 1)

   (c-define 5)

   (with-pg 1)

   (let-fluids with-...)
   (call-with-values 0)
   (let-unspec* 2)

   (run 1)
   (run* 1)
   (fresh 1)
   (conde 0)
   (match 1)
   (receive 2)

   ;; ykk-ports
   (with-current-output-port 1)
   (let-current-output-port 1)
   (maybe-current-output-port 1)
   (let-maybe-current-output-port 1)
   (with-current-input-port 1)
   (let-current-input-port 1)
   (maybe-current-input-port 1)
   (let-maybe-current-input-port 1)
   (with-string-input-port 1)
   (let-string-input-port 1)
   (with-string-ports 1)
   (let-string-ports 1)

   ;; ykk http
   (let-http-response 1)
   (let-headers 1)
   (output-response 2)

   ;; schim
   (with-package 1)
   (with-tag-set 1)
   (with-item 1)

   (cont 2)
   ))

(defun scheme-extend-info (page)
 (require 'info-look)
 (info-lookup-add-help
  :mode 'scheme-mode
  :regexp "[^()`',\" \t\n]+"
  :ignore-case t
  :doc-spec `(("(r5rs)Index" nil "^[ \t]+-+ [^:]+:[ \t]*" "\\b")
              (,page))))

(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
  (lambda ()
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
    (eldoc-mode)))

(provide 'scheme-rc)
