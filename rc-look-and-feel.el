(require 'color-theme)

(defun color-theme-langmartin ()
  (interactive)
  (color-theme-bharadwaj)
  (let ((color-theme-is-cumulative t))
    (color-theme-install
     (let ((match '((t (:background "lightgoldenrod2"))))
           (modeline '((t (:background "grey75" :foreground "black")))))
       `(color-theme-langmartin
         ((background-color . "ivory"))
         nil
         (eshell-prompt ((t (:bold t :foreground "DarkRed"))))
         (isearch ,match)
         (iswitchb-single-match ((t (:inherit font-lock-function-name-face))))
         (font-lock-comment-face ((t (:foreground "grey50"))))
         (font-lock-function-name-face ((t (:foreground "SlateBlue" :slant normal :weight bold))))
         (match ,match)
         (minibuffer-noticeable-prompt ((t (:inherit match))))
         (modeline ,modeline)
         (modeline-buffer-id ((t (:background "LightSlateGrey" :foreground "black"))))
         (modeline-mousable ,modeline)
         (modeline-mousable-minor-mode ,modeline)
         (mode-line-inactive ((t (:background "grey90" :foreground "grey20"))))
         (org-agenda-restriction-lock ((t (:inherit match))))
         (org-clock-overlay ((t (:inherit match))))
         (org-todo ((t (:inherit font-lock-string-face))))
         (trailing-whitespace ((((class color) (background light)) (:background "ivory3"))))
         )))))

(color-theme-initialize)
(color-theme-langmartin)

;; (mapc (lambda (setting)
;;         (update-alist 'default-frame-alist setting))
;;       '((foreground-color . "black")
;;         (background-color . "ivory")))

(defun rc-show-paren-expression ()
  (interactive)
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match "grey95")
  (set-face-background 'show-paren-mismatch "MediumPurple2"))

(defun rc-show-paren-parens ()
  (interactive)
  (setq show-paren-style 'parenthesis)
  (set-face-background 'show-paren-match "grey80")
  (set-face-background 'show-paren-mismatch "purple")
  (set-face-foreground 'show-paren-mismatch "white"))

(rc-show-paren-expression)

(progn
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "ivory"
        term-default-fg-color "black"))

(provide 'rc-look-and-feel)
