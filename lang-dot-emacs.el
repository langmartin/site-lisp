(load "site-start")
(rc-lang)

(require 'js2-mode-rc)

;; things for cocoa emacs 23
(progn
  (require 'mwheel)
  (global-set-key [wheel-down] 'mwheel-scroll)
  (global-set-key [wheel-up] 'mwheel-scroll)
  (global-set-key "\M-`" 'other-frame)
  (require 'tramp)
  (setq tramp-default-proxies-alist
        `(("localhost" "\\`root\\'" nil)
          (nil "\\`root\\'" "/ssh:%h:")))
  (mapc (lambda (x)
          (add-to-list 'tramp-default-method-alist
                       `(,x nil "sshx")))
        '("rove" "abla" "drok"))
  (setq tramp-backup-directory-alist backup-directory-alist))

(desktop-save-mode 1)

(require 'kmacro)

;; (global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
;; (global-set-key "\C-w" 'kill-backward-word-or-region)

(global-set-key "\C-h" 'help)
(global-set-key "\M-h" 'mark-paragraph)

(require 'winner)
(winner-mode 1)

(global-set-key (kbd "C-M-$") 'google-define)
(global-set-key "\C-z" 'undo)

(iswitchb-mode 1)

(setq ring-bell-function nil)

(defvar programming-mode-hooks
  '(c-mode-common-hook
    lisp-mode-hook
    emacs-lisp-mode-hook
    scheme-mode-hook
    perl-mode-hook
    html-mode-hook
    css-mode-hook
    python-mode-hook
    js2-mode-hook))

(add-hooks programming-mode-hooks
           (lambda ()
             ;; (highlight-parentheses-mode 1)
             (highlight-symbol-mode 1)))

(add-hooks '(emacs-lisp-mode-hook python-mode-hook)
           (lambda () (eldoc-mode 1)))

(require 'cx-timesheet)
(rc-emacs-lisp-action)

(require 'lang-scripts)

(require 'smooth-scrolling)

(blink-cursor-mode -1)

(global-set-key "\C-z" 'undo)

(require 'lang-mail-rc)

;; (defun pykk-init ()
;;   (interactive)
;;   (insert-file-contents "~/proj/pykk-interactive.py")
;;   (end-of-buffer)
;;   (comint-send-input))

;; (add-hook 'inferior-python-mode-hook
;;           (lambda () (local-set-key "\C-cp" 'pykk-init)))

(defun end-of-linep () (looking-at "$"))

(progn
  (require 'winner)
  (winner-mode 1))

(global-set-key "\C-x\C-b" 'ibuffer)

(progn
  (require 'term-mode-rc)
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "ivory"
        term-default-fg-color "black"))

(defmacro define-buffer-visitor (visitor-name buffer-name command)
  "http://jfm3-repl.blogspot.com/2009/02/fast-emacs-buffer-flipping.html"
   `(defun ,visitor-name ()
      (interactive)
      (if (get-buffer ,buffer-name)
	  (switch-to-buffer (if (equal ,buffer-name (buffer-name))
				nil
			      ,buffer-name))
	(call-interactively ,command))))

(require 'irc-rc)

(require 'git-commands)

(require 'nav)

(require 'hide-region)

(require 'moz-rc)

(column-number-mode t)

(progn
  (require 'htmlize)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-c." 'org-time-stamp)
  (add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode 1)
              (set-variable 'comment-start nil 'make-local)))
  (require 'org-collector))

(require 'google-define)

(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'asp-rc)

(require 'compile-site-lisp)

(progn
  (require 'color-theme)
  (color-theme-initialize)
  ;; (color-theme-fischmeister)
  ;; (color-theme-subtle-hacker)
  ;; (color-theme-montz)
  ;; (color-theme-shaman)
  ;; (color-theme-bharadwaj)

  (defun color-theme-langmartin ()
    (interactive)
    (color-theme-bharadwaj)
    (let ((color-theme-is-cumulative t))
      (setq isearch nil)
      (color-theme-install
       '(color-theme-langmartin
         ((background-color . "ivory"))
         nil
         (erc-current-nick-face ((t (:bold t :foreground "Turquoise4"))))
         (erc-notice-face ((t (:foreground "grey70"))))
         (erc-timestamp-face ((t (:bold t :foreground "green4"))))
         (eshell-prompt ((t (:bold t :foreground "DarkRed"))))
         (isearch ((t (:inherit match))))
         (iswitchb-single-match ((t (:inherit font-lock-function-name-face))))
         (font-lock-comment-face ((t (:foreground "grey50"))))
         (font-lock-function-name-face ((t (:foreground "SlateBlue" :slant normal :weight bold))))
         (match ((t (:background "lightgoldenrod2"))))
         (minibuffer-noticeable-prompt ((t (:inherit match))))
         (org-agenda-restriction-lock ((t (:inherit match))))
         (org-clock-overlay ((t (:inherit match))))
         (org-todo ((t (:inherit font-lock-string-face))))
         ))))

  (color-theme-langmartin))

(progn
  (global-set-key (kbd "C-x C-c") nil)

  (global-set-key [f1] 'vi-mode)

  (global-set-key [M-f4] 'delete-frame)
  (global-set-key [f5] 'eshell)
  (global-set-key [f6] 'imenu)
  (global-set-key [f7] 'normal-mode)
  (global-set-key [f8] 'toggle-truncate-lines)

  (global-set-key [f10] 'visual-basic-mode)
  (global-set-key [f9] 'css-mode)
  (global-set-key [f11] 'js2-mode)
  (global-set-key [f12] 'rc-sgml-mode-for-asp))

(progn
 (defun split-window-horizontally-minsize (&optional size)
   "Wrapper for spit-window-horizontally that gives 80 columns"
   (interactive "P")
   (let ((size (if size (prefix-numeric-value size)
                 85)))
     (message (format "size: %d" size))
     (split-window-horizontally size)))
 (global-set-key (kbd "C-x 3") 'split-window-horizontally-minsize)
 (setq truncate-partial-width-windows 80))
