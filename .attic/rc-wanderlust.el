;;;; A unicode character representation fix:
;;;; http://osdir.com/ml/mail.wanderlust.general/2008-04/msg00007.html

;;;; 1) apel: Edit Makefile to fix path to emacs, make.
;;;; 2) flim: Edit Makefile, edit FLIM_CFG w/ the path to apel, make.
;;;; 3) semi: Edit Makefile, edit SEMI_CFG w/ paths for apel & flim, make.
;;;; 4) wanderlust: ""

(mapc (lambda (x)
        (add-to-list 'load-path x))
      (list
       ;; http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/semi/
       (rc-contrib "apel-10.7/")
       (rc-contrib "flim-1.14.9/")
       (rc-contrib "semi-1.14.6/")
       ;; :pserver:anonymous@cvs.m17n.org:/cvs/root wanderlust
       (rc-contrib "wanderlust/elmo/")
       (rc-contrib "wanderlust/wl/")
       ))

(add-to-list 'Info-directory-list (rc-contrib "wanderlust/doc/"))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "lang.martin@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "lang.martin")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

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

(provide 'rc-wanderlust)
