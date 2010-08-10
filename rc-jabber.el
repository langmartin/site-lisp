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

(defun jabber ()
  "The acccount list logic is all built in to the interactive block of jabber-connect. This function duplicates that functionality, reading the jabber-account-default entry of jabber-account-list and passing the arguments to jabber-connect."
  (interactive)
  (save-default-directory
      "~"
    (set-buffer (current-buffer))
    (and-let* ((acct (assoc jabber-account-default jabber-account-list))
               (jid (car acct))
               (acct (cdr acct)))
      (let ((username (jabber-jid-username jid))
            (server (jabber-jid-server jid))
            (resource (jabber-jid-resource jid))
            (network-server (cdr (assq :network-server acct)))                              (port (cdr (assq :port acct)))
            (connection-type (cdr (assq :connection-type acct)))
            (password (cdr (assq :password acct))))
        (jabber-connect username server resource
                        nil
                        password network-server port connection-type)))))

(defvar jabber-activity-switched-from nil
  "The last non-jabber buffer I switched from")

(defun switch-to-active-jabber-buffer ()
  (let ((buf (current-buffer)))
    (jabber-activity-switch-to)
    (if (eq buf (current-buffer))
        nil
      (progn
        (when (not (eq major-mode 'jabber-chat-mode))
          (setq jabber-activity-switched-from buf))
        t))))

(if (not (boundp 'erc-modified-channels-alist))
    (setq erc-modified-channels-alist nil))

(defun switch-to-active-chat-buffer ()
  "Switch to any jabber activity, then switch to active erc buffers."
  (interactive)
  (or (switch-to-active-jabber-buffer)
      (and (or erc-modified-channels-alist
               (eq major-mode 'erc-mode))
           (erc-track-switch-buffer 1))
      (and (eq major-mode 'jabber-chat-mode)
           jabber-activity-switched-from
           (bury-buffer (current-buffer))
           (switch-to-buffer jabber-activity-switched-from))))

(progn
  (defvar switch-to-active-chat-map (make-sparse-keymap))

  (define-key switch-to-active-chat-map
    (kbd "C-c C-@") 'switch-to-active-chat-buffer)
  (define-key switch-to-active-chat-map
    (kbd "C-c C-SPC") 'switch-to-active-chat-buffer)

  (define-minor-mode switch-to-active-chat-minor-mode
    :init-value nil
    :keymap switch-to-active-chat-map
    :global t))

(setq erc-track-enable-keybindings nil)
(switch-to-active-chat-minor-mode 1)
(provide 'rc-jabber)
