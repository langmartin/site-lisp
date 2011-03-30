(defun rc-color-theme-langmartin ()
  "Install a full on color theme using color-theme.el."
  (interactive)
  (require 'color-theme)

  (progn
    ;; http://www.emacswiki.org/emacs/ColorThemeQuestions#toc2
    (defun color-theme-undo ()
     (interactive)
     (color-theme-reset-faces)
     (color-theme-snapshot))
    (fset 'color-theme-snapshot (color-theme-make-snapshot)))

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
  (color-theme-langmartin)
  (rc-show-paren-expression))

(defun rc-custom-faces-langmartin ()
  "Minor modifications of the default theme by way of custom-set-faces."
  (interactive)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(eshell-prompt ((((class color) (background light)) (:foreground "Red4" :weight bold))))
   '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "grey50"))))
   '(font-lock-warning-face ((((class color) (min-colors 88) (background light)) (:foreground "Red3" :weight bold))))
   '(jabber-chat-prompt-foreign ((t (:foreground "red4"))))
   '(jabber-chat-prompt-local ((t (:foreground "blue4"))))
   '(jabber-chat-prompt-system ((t (:foreground "green4" :weight bold))))
   '(org-mode-line-clock-overrun ((t (:inherit modeline :background "grey" :foreground "red3"))) t)
   '(org-todo ((((class color) (min-colors 16) (background light)) (:foreground "Red4" :weight bold))))
   '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "LemonChiffon"))))
   '(whitespace-line ((t (:background "gray90"))))
   '(whitespace-space-after-tab ((t (:background "lightyellow" :foreground "firebrick")))))
  (rc-show-paren-expression))

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

(defun rc-emacs-master-no-theme ()
  "A bunch of settings that should only be applied to my primary,
long-running emacs session. It's useful to have these optional so
that I can avoid a bunch of warnings if I open a new emacs to run
something that needs a second thread."
  (interactive)
  (require 'edit-server)
  (edit-server-start)
  (server-mode 1)
  (savehist-mode 1)
  (setq desktop-dirname "~/.emacs.d")
  (desktop-save-mode 1)
  (desktop-read)
  (global-set-keys '(("C-x C-c" . nil))))

(defun rc-emacs-master ()
  "See rc-emacs-master-no-theme. This one has a theme, too."
  (interactive)
  (rc-emacs-master-no-theme)
  (rc-custom-faces-langmartin))

(defun rc-emacs-compile ()
  (interactive)
  (savehist-mode 1)
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-zenburn))

(provide 'rc-look-and-feel)
