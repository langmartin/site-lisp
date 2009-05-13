;;; things for cocoa emacs 23
(progn
  (require 'mwheel)
  (global-set-key [wheel-down] 'mwheel-scroll)
  (global-set-key [wheel-up] 'mwheel-scroll)
  (global-set-key "\M-`" 'other-frame)
  (require 'tramp))

(load "site-start")

(progn
 (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

 (defun irc () "using erc"
   (interactive)
   (erc-select :server "irc.freenode.net"
               :port 6667
               :nick "langmartin"

               :full-name "Lang Martin"))

 (add-hook 'erc-text-matched-hook 'erc-growl-match)
 (erc-match-mode 1)

 (defun erc-hide-notices () "hide all notices in a very busy channel"
   (interactive)
   (make-local-variable 'erc-echo-notice-always-hook)
   (setq erc-echo-notice-always-hook nil)))

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

(rc-lang)
(global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
(global-set-key "\C-w" 'kill-backward-word-or-region)
(global-set-key "\C-h" 'help)

(iswitchb-mode 1)

(setq ring-bell-function nil)

(setq tramp-backup-directory-alist backup-directory-alist)

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

(rc-screen-ify-control-t 'global-set-key)

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

;;; (progn
;;;   (require 'bm)
;;;   (global-set-key [kp-3] 'bm-toggle)
;;;   (global-set-key [kp-6] 'bm-next)
;;;   (global-set-key [kp-9] 'bm-previous)
;;;   (global-set-key [kp-5] 'bm-show-all))

(require 'rc-term-mode)
(require 'rc-anything)
(require 'w3m)
