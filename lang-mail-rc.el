(defvar smtpmail-account-authinfo
  '(("lang.martin@gmail.com" . "~/.emacs.d/authinfo-gmail")
    ;; ("lang.martin@coptix.com" . "~/.emacs.d/authinfo-coptix")
    ))

(defun message-extract-from-address ()
  (let ((from (save-excursion
               (save-restriction
                 (message-narrow-to-headers)
                 (message-fetch-field "from")))))
    (string-match "<\\(.*?\\)>" from)
    (match-string 1 from)))

(defun smtpmail-through-matching-account ()
  "Change the SMTP server according to the current from line."
  (interactive)
  (save-excursion
    (let* ((from (message-extract-from-address))
           (auth (assoc from smtpmail-account-authinfo))
           (auth (if auth (cdr auth))))
      (message "From is `%s', setting `smtpmail-auth-credentials' to `%s'"
               from
               auth)
      (setq smtpmail-auth-credentials auth))))

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
        ;; '((nnimap "noc"
        ;;           (nnimap-address "noc.imap.coptix.com")
        ;;           (nnimap-server-port 993)
        ;;           (nnimap-stream ssl))
        ;;   (nnimap "coptix"
        ;;           (nnimap-address "imap.coptix.com")
        ;;           (nnimap-server-port 993)
        ;;           (nnimap-stream ssl)))
        nil)
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq gnus-use-full-window nil)
  (setq gnus-posting-styles
        '(("." (address "lang.martin@gmail.com"))
          ;; ("coptix:" (address "lang.martin@coptix.com"))
          ))
  (progn
    (require 'starttls)
    (require 'smtpmail)
    (setq user-mail-address "lang.martin@gmail.com")
    (setq smtpmail-smtp-default-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" ""))
          smtpmail-sendto-domain "gmail.com")
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it)
    ;; (setq smtpmail-debug-info nil smtpmail-debug-verb nil)
    (add-hook 'mail-mode-hook
              (lambda () (auto-fill-mode t)))
    (add-hook 'message-setup-hook
              'smtpmail-through-matching-account)))

(rc-gnus)

(provide 'lang-mail-rc)
