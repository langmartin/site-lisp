(with-feature rcirc
  (require 'rc-rcirc))

(require 'rc-erc)

(with-feature jabber
 (require 'rc-jabber))

;;;; Switch to active jabber buffers then to active erc buffers on C-c
;;;; C-space.
(defun currently-chattingp ()
  (or (eq major-mode 'jabber-chat-mode)
      (eq major-mode 'erc-mode)
      (eq major-mode 'rcirc-mode)))

(defmacro progt (&rest body)
  "Execute a sequence like progn, but return t"
  `(progn
     ,@body
     t))

;; (defun switch-to-active-chat-buffer1 (prefix)
;;   (if jabber-activity-jids
;;       (progt (jabber-activity-switch-to))
;;     (if (rcirc-split-activity rcirc-activity)
;;         (progt (rcirc-next-active-buffer prefix))
;;       nil)))

(defun switch-to-active-chat-buffer1 (prefix)
  (if jabber-activity-jids
      (progt (jabber-activity-switch-to))
    (if erc-modified-channels-alist
        (progt (erc-track-switch-buffer 1))
      nil)))

(defvar switch-to-active-chat-buffer-last nil)

(defun switch-to-active-chat-buffer (prefix)
  "Switch to any jabber activity, then switch to active erc buffers."
  (interactive "P")
  (if (currently-chattingp)
      (or (switch-to-active-chat-buffer1 prefix)
          (when switch-to-active-chat-buffer-last
            (switch-to-buffer switch-to-active-chat-buffer-last)))
    (progn
      (setq switch-to-active-chat-buffer-last (current-buffer))
      (or (switch-to-active-chat-buffer1 prefix)
          (message "No active chat buffers.")))))

(unless (boundp 'erc-modified-channels-alist)
  (setq erc-modified-channels-alist nil))
(unless (boundp 'jabber-activity-jids)
  (setq jabber-activity-jids nil))

(defvar switch-to-active-chat-map (make-sparse-keymap))

(let ((map switch-to-active-chat-map))
  (define-key map (kbd "C-c C-@") 'switch-to-active-chat-buffer)
  (define-key map (kbd "C-c C-SPC") 'switch-to-active-chat-buffer))

(define-minor-mode switch-to-active-chat-minor-mode
  :init-value nil
  :keymap switch-to-active-chat-map
  :global t)

(setq rcirc-track-minor-mode-map (make-sparse-keymap))
(setq erc-track-enable-keybindings nil)
(switch-to-active-chat-minor-mode 1)

(provide 'rc-chat)
