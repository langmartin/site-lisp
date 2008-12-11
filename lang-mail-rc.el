(defun mail-send-and-exit-kill (&optional arg)
  "send mail message, bury the buffer and kill it. See mail-send-and-exit."
  (interactive "P")
  (let ((mail-buffer (current-buffer)))
    (mail-send)
    (mail-bury arg)
    (kill-buffer mail-buffer)))

;; (defun rc-gmail-smtp ()
;;   (interactive)
;;   (setq user-mail-address "lang.martin@gmail.com")
;;   (setq smtpmail-auth-credentials "~/.emacs.d/authinfo-gmail"))

(progn
  (require 'starttls)
  (require 'smtpmail)
  (setq user-mail-address "lang.martin@coptix.com"
        smtpmail-auth-credentials "~/.emacs.d/authinfo-coptix")
  (setq smtpmail-smtp-default-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" ""))
        smtpmail-sendto-domain "coptix.com")
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  ;; (setq smtpmail-debug-info nil smtpmail-debug-verb nil)
  (add-hook 'mail-mode-hook
            (lambda () (auto-fill-mode t))))

(defun rc-gnus ()
  (require 'gnus)
  (setq gnus-invalid-group-regexp "[:`'\"]\\|^$"
        gnus-ignored-newsgroups "")
  (require 'nnimap)
  (setq gnus-nntp-server nil)
  (setq gnus-select-method
        '(nnimap "coptix"
                 (nnimap-address "imap.coptix.com")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  ;; (setq gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))
  ;; (setq gnus-agent nil)
  ;; (add-hooks '(gnus-summary-mode-hook)
  ;;              (lambda ()
  ;;                (rc-screen-ify-control-t 'local-set-key)))
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  ;; (setq gnus-auto-expirable-newsgroups "Christmas\\|INBOX\\|Starred")
  )

(rc-gnus)

(provide 'lang-mail-rc)

(defun rc-wanderlust ()
  "I don't care for wanderlust anymore, but this is what I tried once"
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
  (setq wl-highlight-folder-with-icon nil))
