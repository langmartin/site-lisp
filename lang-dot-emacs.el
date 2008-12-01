(load "site-start")

;;; (require 'w3m-load)

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
  (setq erc-echo-notice-always-hook nil))

(require 'kmacro)

(defun color-grey () (color-theme-fischmeister) t)
(defun color-slate () (color-theme-pok-wog) t)
(and (require 'color-theme "color-theme" t)
     (or (color-theme-initialize)
         (color-grey)
         (color-slate)))

(rc-lang)
(global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
(global-set-key "\C-w" 'kill-backward-word-or-region)
(global-set-key "\C-h" 'help)

(require 'mwheel)
(global-set-key [wheel-down] 'mwheel-scroll)
(global-set-key [wheel-up] 'mwheel-scroll)

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

(require 'rc-term-mode)

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

(defun end-of-linep ()
  (looking-at "$"))

(defun underline-this-line (fill-character)
  (interactive "cfill character:")
  (let ((count 0))
    (save-excursion
      (beginning-of-line)
      (previous-line)
      (while (not (end-of-linep))
        (setq count (+ count 1))
        (forward-char)))
    (dotimes (ii count)
      (insert fill-character))))

(global-set-key [kp-4] 'underline-this-line)

(require 'toggle-letter-case)

(require 'less)
(add-hook 'find-file-hooks 'auto-less-minor-mode)

(global-set-key "\M-`" 'other-frame)

(progn
  (require 'winner)
  (winner-mode 1))