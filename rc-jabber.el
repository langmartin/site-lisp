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

;;;; Switch to active jabber buffers then to active erc buffers on C-c
;;;; C-space.
(defun currently-chattingp ()
  (or (eq major-mode 'jabber-chat-mode)
      (eq major-mode 'erc-mode)))

(defmacro progt (&rest body)
  "Execute a sequence like progn, but return t"
  `(progn
     ,@body
     t))

(defun switch-to-active-chat-buffer1 ()
  (if jabber-activity-jids
      (progt (jabber-activity-switch-to))
    (if erc-modified-channels-alist
        (progt (erc-track-switch-buffer 1))
      nil)))

(defvar switch-to-active-chat-buffer-last nil)

(defun switch-to-active-chat-buffer ()
  "Switch to any jabber activity, then switch to active erc buffers."
  (interactive)
  (if (currently-chattingp)
      (or (switch-to-active-chat-buffer1)
          (when switch-to-active-chat-buffer-last
            (switch-to-buffer switch-to-active-chat-buffer-last)))
    (progn
      (setq switch-to-active-chat-buffer-last (current-buffer))
      (or (switch-to-active-chat-buffer1)
          (message "No active chat buffers.")))))

(progn
  (unless (boundp 'erc-modified-channels-alist)
    (setq erc-modified-channels-alist nil))
  (unless (boundp 'jabber-activity-jids)
    (setq jabber-activity-jids nil))

  (defvar switch-to-active-chat-map (make-sparse-keymap))
  (define-key switch-to-active-chat-map
    (kbd "C-c C-@") 'switch-to-active-chat-buffer)
  (define-key switch-to-active-chat-map
    (kbd "C-c C-SPC") 'switch-to-active-chat-buffer)

  (define-minor-mode switch-to-active-chat-minor-mode
    :init-value nil
    :keymap switch-to-active-chat-map
    :global t)

  (setq erc-track-enable-keybindings nil)
  (switch-to-active-chat-minor-mode 1))

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

(provide 'rc-jabber)
