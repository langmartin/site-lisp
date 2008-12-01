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
