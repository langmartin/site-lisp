(progn
  (add-to-list 'load-path "~/Contrib/emacs-jabber-0.8.0/")
  (require 'jabber)
  (require 'jabber-autoloads)
  (add-to-list 'Info-directory-list "~/Contrib/emacs-jabber-0.8.0/"))

(set-variables
 '(jabber-account-list (quote (("lang.martin@gmail.com" (:network-server . "talk.google.com") (:connection-type . ssl)))))
 '(jabber-alert-presence-hooks nil)
 '(jabber-vcard-avatars-retrieve nil))

(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "red4"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue4")))))

(provide 'rc-jabber)
