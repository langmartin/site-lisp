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

(defun rc-gnus ()
  (require 'gnus)
  (setq gnus-invalid-group-regexp "[:`'\"]\\|^$"
        gnus-ignored-newsgroups "")
  (require 'nnimap)
  (setq gnus-select-method
        '(nnimap "coptix"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  (setq gnus-secondary-select-methods
        '((nntp "newsgroups.bellsouth.net")))
  (setq gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))
  (add-hooks '(gnus-summary-mode-hook)
             (lambda ()
               (rc-screen-ify-control-t 'local-set-key))))

(defun mail-send-and-exit-kill (&optional arg)
  "send mail message, bury the buffer and kill it. See mail-send-and-exit."
  (interactive "P")
  (let ((mail-buffer (current-buffer)))
    (mail-send)
    (mail-bury arg)
    (kill-buffer mail-buffer)))

(defun rc-gmail-smtp ()
  (require 'starttls)
  (require 'smtpmail)
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-auth-credentials "~/.authinfo"
        smtpmail-sendto-domain "coptix.com"
        smtpmail-smtp-service 587
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  ;; (setq smtpmail-debug-info nil smtpmail-debug-verb nil)
  (add-hooks '(mail-mode-hook)
             (lambda ()
               (auto-fill-mode t)
               (local-set-key "\C-c\C-c" 'mail-send-and-exit-kill))))

(rc-gmail-smtp)

(defun pykk-init ()
  (interactive)
  (insert-file-contents "~/proj/pykk-interactive.py")
  (end-of-buffer)
  (comint-send-input))

(add-hook 'inferior-python-mode-hook
          (lambda () (local-set-key "\C-cp" 'pykk-init)))

(defun rc-wanderlust ()
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

  ;; IMAP
  (setq elmo-imap4-default-server "imap.gmail.com")
  (setq elmo-imap4-default-user "lang.martin@coptix.com") 
  (setq elmo-imap4-default-authenticate-type 'clear) 
  (setq elmo-imap4-default-port '993)
  (setq elmo-imap4-default-stream-type 'ssl)
  (setq elmo-imap4-use-modified-utf7 t) 

  ;; SMTP
  (setq wl-smtp-connection-type 'starttls)
  (setq wl-smtp-posting-port 587)
  (setq wl-smtp-authenticate-type "plain")
  (setq wl-smtp-posting-user "lang.martin@coptix.com")
  (setq wl-smtp-posting-server "smtp.gmail.com")
  (setq wl-local-domain "coptix.com")

  (setq wl-default-folder "%inbox")
  (setq wl-default-spec "%")
  (setq wl-draft-folder ".drafts")
  (setq wl-trash-folder ".trash")

  (setq wl-folder-check-async t) 

  (setq elmo-imap4-use-modified-utf7 t)

  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

  (setq wl-icon-directory
        "/Applications/Emacs.app/Contents/Resources/etc/wl/icons")
  (setq wl-highlight-folder-with-icon nil)
  )

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
