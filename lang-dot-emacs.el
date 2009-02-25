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

(progn
 (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
 (add-hook 'erc-text-matched-hook 'erc-growl-match)
 (erc-match-mode 1)
 (defun erc-hide-notices () "hide all notices in a very busy channel"
   (interactive)
   (make-local-variable 'erc-echo-notice-always-hook)
   (setq erc-echo-notice-always-hook nil))
 (defalias 'irc 'erc))

(require 'kmacro)

(progn
 (defun color-grey () (color-theme-fischmeister) t)
 (defun color-slate () (color-theme-subtle-hacker) t)
 (and (require 'color-theme "color-theme" t)
      (progn (color-theme-initialize)
             ;; (color-slate)
             ;; (show-paren-mode nil)
             (color-grey)
             )))

;; (global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
(global-set-key "\C-w" 'kill-backward-word-or-region)
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

(global-set-key "\C-z" nil)

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
  (require 'rc-term-mode)
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "gray80"
        term-default-fg-color "black"))

;; (require 'rc-anything)
;; (require 'w3m) ; pushes the os x menus right

;; (progn
;;   (require 'timeclock-x)
;;   (display-time-mode)
;;   (setq timeclock-query-project-interval (* 60 90))
;;   (timeclock-query-project-on)
;;   (timeclock-modeline-display 1)
;;   (add-hook 'emacs-startup-hook 'timeclock-query-in)
;;   (timeclock-setup-keys))

(progn
  (require 'timeclock)
  (define-key ctl-x-map "ti" 'timeclock-in)
  (define-key ctl-x-map "to" 'timeclock-out)
  (define-key ctl-x-map "tc" 'timeclock-change)
  (define-key ctl-x-map "tr" 'timeclock-reread-log)
  (define-key ctl-x-map "tv" 'timeclock-visit-timelog)
  (define-key ctl-x-map "ts" 'timeclock-status-string)
  (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string))
