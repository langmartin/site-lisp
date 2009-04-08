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

(defun rc-mew ()
  (require 'mew)
  ;; (progn
  ;;     (autoload 'mew-user-agent-compose "mew" nil t)
  ;;     (if (boundp 'mail-user-agent)
  ;;         (setq mail-user-agent 'mew-user-agent))
  ;;     (if (fboundp 'define-mail-user-agent)
  ;;         (define-mail-user-agent
  ;;           'mew-user-agent
  ;;           'mew-user-agent-compose
  ;;           'mew-draft-send-message
  ;;           'mew-draft-kill
  ;;           'mew-send-hook)))
  ;; (setq mew-name "your name") ;; (user-full-name)
  (setq mew-user "lang.martin")
  (setq mew-mail-domain "coptix.com")
  (setq mew-proto "%")
  (setq mew-imap-user "lang.martin@coptix.com")
  (setq mew-imap-server "imap.coptix.com")
  (setq mew-prog-ssl "/coptix/local/bin/stunnel")
  (setq mew-ssl-ver 4.23)
  (setq mew-imap-ssl t)
  (setq mew-imap-ssl-port 993))
;; (rc-mew)
