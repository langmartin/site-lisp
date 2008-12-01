(defun mail-send-and-exit-kill (&optional arg)
  "send mail message, bury the buffer and kill it. See mail-send-and-exit."
  (interactive "P")
  (let ((mail-buffer (current-buffer)))
    (mail-send)
    (mail-bury arg)
    (kill-buffer mail-buffer)))

(progn
  (require 'starttls)
  (require 'smtpmail)
  (setq smtpmail-smtp-default-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" ""))
        smtpmail-sendto-domain "coptix.com")
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  ;; (setq smtpmail-debug-info t smtpmail-debug-verb t)
  (add-hooks '(mail-mode-hook)
             (lambda ()
               (auto-fill-mode t)
               (local-set-key "\C-c\C-c" 'mail-send-and-exit-kill))))

(defun rc-coptix-smtp ()
  (interactive)
  (setq user-mail-address "lang.martin@coptix.com")
  (setq smtpmail-auth-credentials "~/.emacs.d/authinfo-coptix"))

(defun rc-gmail-smtp ()
  (interactive)
  (setq user-mail-address "lang.martin@gmail.com")
  (setq smtpmail-auth-credentials "~/.emacs.d/authinfo-gmail"))

(rc-coptix-smtp)

(defun rc-gnus ()
  (require 'gnus)
  (setq gnus-invalid-group-regexp "[:`'\"]\\|^$"
        gnus-ignored-newsgroups "")
  (require 'nnimap)
  (setq gnus-nntp-server nil)
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  (setq gnus-secondary-select-methods
        '(;; (nntp "newsgroups.bellsouth.net")
          (nnimap "coptix"
                   (nnimap-address "imap.coptix.com")
                   (nnimap-server-port 993)
                   (nnimap-stream ssl))))
  ;; (setq gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))
  ;; (setq gnus-agent nil)
  (add-hooks '(gnus-summary-mode-hook)
             (lambda ()
               (rc-screen-ify-control-t 'local-set-key)))
  (setq mm-discouraged-alternatives '("text/html" "text/richtext")))

(rc-gnus)

(provide 'lang-mail-rc)
