(load "site-start")
(rc-lang)

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
    (let ((color-theme-is-cumulative t)
          (match '((t (:background "lightgoldenrod2"))))
          (string '((t (:foreground "DarkRed"))))
          (string-b '((t (:bold t :foreground "DarkRed"))))
          (comment '((t (:foreground "grey50"))))
          )
      (color-theme-install
       `(color-theme-langmartin
         ((background-color . "ivory"))
         nil
         (erc-current-nick-face ((t (:bold t :foreground "Turquoise4"))))
         (erc-notice-face ,comment)
         (erc-timestamp-face ((t (:bold t :foreground "green4"))))
         (eshell-prompt ,string-b)
         (font-lock-comment-face ,comment)
         (font-lock-function-name-face ((t (:bold t :foreground "SlateBlue"))))
         (match ,match)
         (minibuffer-noticeable-prompt ,match)
         (org-agenda-restriction-lock ,match)
         (org-clock-overlay ,match)
         (org-todo ,string)
         ))))
  
  (color-theme-langmartin)
  )

;; (global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
;; (global-set-key "\C-w" 'kill-backward-word-or-region)
(global-set-key "\C-h" 'help)

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
    python-mode-hook))

(add-hooks programming-mode-hooks
           (lambda ()
             ;; (highlight-parentheses-mode 1)
             (highlight-symbol-mode 1)))

(add-hooks '(emacs-lisp-mode-hook python-mode-hook)
           (lambda () (eldoc-mode 1)))

(add-hooks '(javascript-mode-hook)
           (lambda () (local-set-key "\C-m" 'newline)))

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

;; (require 'toggle-letter-case)

(require 'less)
;; (add-hook 'find-file-hooks 'auto-less-minor-mode)

(progn
  (require 'winner)
  (winner-mode 1))

(global-set-key "\C-x\C-b" 'ibuffer)

(progn
  (require 'term-mode-rc)
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "gray80"
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

(require 'timeclock-rc)

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
              (auto-fill-mode 1)))
  (require 'org-collector))

(require 'google-define)

(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'asp-rc)
