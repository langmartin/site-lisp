(progn
  (add-to-list 'load-path "~/Contrib/emacs-jabber-0.8.0/")
  (require 'jabber)
  (require 'jabber-autoloads)
  (add-to-list 'Info-directory-list "~/Contrib/emacs-jabber-0.8.0/"))

(set-variables
 '(jabber-alert-presence-hooks nil)
 '(jabber-vcard-avatars-retrieve nil))

(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "red4"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue4")))))

(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

(provide 'rc-jabber)
