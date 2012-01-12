;; (add-to-list 'load-path (rc-contrib "emacs-jabber-0.8.0/"))
;; (add-to-list 'Info-directory-list (rc-contrib "emacs-jabber-0.8.0/"))

(progn
  (require 'jabber)
  (require 'jabber-autoloads))

;; (add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)

(set-variables
 '(jabber-alert-presence-hooks nil)
 '(jabber-vcard-avatars-retrieve nil))

(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "red4"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue4"))))
 '(jabber-chat-prompt-system ((t (:foreground "green4" :weight bold)))))

(defun rc-jabber-mush-in-passwords ()
  (interactive)
  (if (boundp 'jabber-account-passwords)
      (setq jabber-account-list
            (mapcar (lambda (acct)
                      (let ((pass (member-alist (car acct) jabber-account-passwords)))
                        (if (not (null pass))
                            (cons (car acct)
                                  (cons (cons :password pass)
                                        (cdr acct)))
                          acct)))
                    jabber-account-list))))

(rc-jabber-mush-in-passwords)

(provide 'rc-jabber)
