(defvar smtpmail-account-authinfo
  `(("@gmail.com" . "~/.emacs.d/authinfo-gmail")
    ("@work.com" . "~/.emacs.d/authinfo-work")
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
  (setq gnus-always-read-dribble-file t)
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  (setq gnus-secondary-select-methods
        ;;   (nnimap "work"
        ;;           (nnimap-address "imap.work.com")
        ;;           (nnimap-server-port 993)
        ;;           (nnimap-stream ssl)))
        nil)
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq gnus-use-full-window nil)
  (setq gnus-posting-styles
        `(("." (address ,email-private))
          ;; ("work:" (address ,email-work))
          ))
  (progn
    (require 'starttls)
    (require 'smtpmail)
    (setq user-mail-address email-private)
    (setq smtpmail-smtp-default-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" ""))
          smtpmail-sendto-domain "gmail.com")
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it)
    ;; (setq smtpmail-debug-info nil smtpmail-debug-verb nil)
    (add-hook 'mail-mode-hook 'visual-line-not-auto-fill)
    (add-hook 'message-mode-hook 'visual-line-not-auto-fill)
    (add-hook 'message-setup-hook
              'smtpmail-through-matching-account))
  (progn
    ;; Speed tricks: http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
    (gnus-compile)
    (setq gc-cons-threshold 3500000)
    (setq gnus-use-correct-string-widths nil)))

(defun visual-line-not-auto-fill ()
  (interactive)
  (auto-fill-mode nil)
  (visual-line-mode t))

(rc-gnus)

(provide 'gnus-rc)
