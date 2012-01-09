(progn
  ;; settings for bleeding-ier edge No Gnus v0.11
 (setq gnus-registry-install t)
 (setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
 (setq gnus-message-archive-group nil)
 )

(require 'message)
(require 'advice)

(defun rc-smtpmail-through-matching-accounts ()
  "Doesn't work in emacs 24."

(defvar smtpmail-account-authinfo '())

(defun message-extract-header (header)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-fetch-field header))))

(defun message-extract-from-address ()
  (let ((from (message-extract-header "from")))
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

;; (define-key message-mode-map (kbd "C-c C-g") 'smtpmail-through-matching-account)

(defadvice message-send (around message-send-check activate)
  (smtpmail-through-matching-account)
  (let ((from (message-extract-from-address))
        (domain (concat "@" email-work-domain)))
    (save-excursion
      (save-restriction
        (message-narrow-to-headers)
        (point-min)
        (if (search-forward domain nil t)
            (if (not (string-match domain from))
                (error "You sent this from the wrong account.")))
        ad-do-it))))

  (progn
    (require 'starttls)
    (require 'smtpmail)
    (setq smtpmail-smtp-default-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" ""))
          smtpmail-sendto-domain nil    ;"gmail.com"
          )
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it)
    ;; (setq smtpmail-debug-info nil smtpmail-debug-verb nil)
    )
)

(defun rc-gnus ()
  (require 'gnus)
  (setq gnus-invalid-group-regexp "[:`'\"]\\|^$"
        gnus-ignored-newsgroups "")
  (require 'nnimap)
  (setq gnus-nntp-server nil)
  (setq gnus-always-read-dribble-file t)
  (require 'nnir)
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)
                 (nnir-search-engine imap)))
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))
  (setq gnus-use-full-window nil)
  ;; (setq gnus-posting-styles
  ;;       `(("." (address ,user-mail-address))
  ;;         ;; ("work:" (address ,email-work))
  ;;         ))
  (progn
    (setq gnus-treat-display-smileys nil)
    ;; (add-hook 'mail-mode-hook 'visual-line-not-auto-fill)
    ;; (add-hook 'message-mode-hook 'visual-line-not-auto-fill)
    ;; (add-hook 'message-setup-hook 'smtpmail-through-matching-account)
    (set-default 'mail-user-agent 'gnus-user-agent)
    (setq gnus-novice-user nil))
  (progn
    ;; Speed tricks: http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
    (gnus-compile)
    (setq gc-cons-threshold 3500000)
    (setq gnus-use-correct-string-widths nil))
  )

(defun message-cite-pgp-sign ()
  "Get the PGP signature block stuck at the top of the message where it will pick up correctly on send."
  (interactive)
  (message-cite-original-without-signature)
  (save-excursion
    (message-goto-body)
    (mml-secure-message-sign-pgpmime)))

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(setq message-cite-function 'message-cite-pgp-sign)

(rc-gnus)

(set-variables
 '(gnus-build-sparse-threads (quote some))
 '(gnus-dribble-directory "~/.emacs.d")
 '(gnus-fetch-old-headers (quote invisible))
 '(gnus-refer-thread-limit t)
 '(gnus-message-replysign t)
 '(gnus-message-replyencrypt t)
 '(mail-mailing-lists
   (quote ("gambit-list@iro.umontreal.ca"
           "all@coptix.com"
           "dns@list.cr.yp.to"
           "cfpug@cfpug.com"
           ))))

(defun rc-bbdb ()
  (require 'bbdb-autoloads nil t)
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (bbdb-insinuate-message)
  (bbdb-insinuate-gnus)
  (custom-set-variables
   '(bbdb-complete-name-allow-cycling t)
   '(bbdb-dwim-net-address-allow-redundancy t)
   '(bbdb-file "~/.emacs.d/bbdb")))

(rc-bbdb)

(require 'gnus-notify)
;; Put your cursor on "All Mail" G p add (modline-notify t) to the list

(defun gnus-group-restart-dont-ask ()
  (interactive)
  (save-default-directory
      "~"
    (flet ((gnus-yes-or-no-p (prompt) t))
      (gnus-group-restart)
      ;; Add a demon command to check for new mail
      ;; (gnus-demon-add-handler 'gnus-group-get-new-news 5 1)
      )))

(global-set-key (kbd "C-x m")
                (alist-to-keymap-via-kbd
                 '(("m" . compose-mail)
                   ("r" . gnus-group-restart-dont-ask))))

(defun rc-smtp-through-msmtp ()
  (interactive)
  (custom-set-variables
   '(message-send-mail-function 'message-send-mail-with-sendmail)
   '(sendmail-program (executable-find "msmtp"))
   '(message-sendmail-f-is-evil t)
   '(message-sendmail-extra-arguments '("--read-envelope-from"))))

(rc-smtp-through-msmtp)

(provide 'rc-gnus)
