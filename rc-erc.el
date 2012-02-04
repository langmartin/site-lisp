(require 'erc)
(define-key erc-mode-map "\C-c\C-x" (make-sparse-keymap))

(defun erc-fix-colors ()
  (interactive)
  (set-face-foreground 'erc-current-nick-face "Turquoise4")
  (set-face-foreground 'erc-notice-face "grey70")
  (set-face-foreground 'erc-timestamp-face "green4")
  (set-face-foreground 'erc-pal-face "green4")
  (set-face-attribute  'erc-pal-face nil :weight 'bold)
  (set-face-foreground 'erc-prompt-face nil)
  (set-face-background 'erc-prompt-face nil)
  ;; (set-face-foreground 'erc-my-nick-face nil)
  (set-face-foreground 'erc-my-nick-face "brown")
  (set-face-foreground 'erc-input-face nil))

(defun irc-bitlbee ()
  (interactive)
  (erc :server "localhost"
       :port 6667
       :nick "langmartin"
       :full-name "Lang Martin"
       :password bitlbee-password)
  (erc-fix-colors))

(set-variables
 '(erc-join-hook (quote (bitlbee-identify))))

(defun erc-hide-notices () "hide all notices in a very busy channel"
  (interactive)
  (make-local-variable 'erc-echo-notice-always-hook)
  (setq erc-echo-notice-always-hook nil))

;; (setq erc-autojoin-channels-alist
;;        '(("freenode.net" "#emacs" "#scheme" "#medium")))

(defun irc-freenode ()
  (interactive)
  (erc :server "irc.freenode.net"
       :nick "langmartin"
       :full-name "Lang Martin"
       :port 8001
       :password freenode-password)
  (erc-fix-colors))

;; (define-buffer-visitor visit-medium "#medium" 'irc)
;; (global-set-key (kbd "H-m") 'visit-medium)

(defun irc ()
  (interactive)
  (save-default-directory
      "~"
    (irc-freenode)))

(custom-set-variables
 '(erc-autoaway-mode t)
 '(erc-autoaway-idle-method (quote user))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-join-buffer (quote bury))
 '(erc-log-channels-directory "~/.emacs.d/log")
 '(erc-match-mode 1)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-pals (quote ("jlongster" "bweaver" "timmywil")))
 '(erc-server-reconnect-timeout 300)
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-user-full-name "Lang Martin")
 ;; http://www.bestinclass.dk/index.php/2010/03/approaching-productivity/
 '(erc-button-url-regexp
   "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")
 )

(add-hook 'erc-disconnected-hook
          (lambda (nick ip reason)
            (erc-log-save-all-buffers)))

(defun erc-growl-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat "ERC " (buffer-name (current-buffer)))
     message)))

;; (add-hook 'erc-text-matched-hook 'erc-growl-hook)

(provide 'rc-erc)
