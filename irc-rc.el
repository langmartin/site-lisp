(require 'erc)
(define-key erc-mode-map "\C-c\C-x" (make-sparse-keymap))

(defun erc-fix-colors ()
  (interactive)
  (set-face-foreground 'erc-current-nick-face "Turquoise4")
  (set-face-foreground 'erc-notice-face "grey70")
  (set-face-foreground 'erc-timestamp-face "green4")
  (set-face-foreground 'erc-pal-face "green4")
  (set-face-attribute 'erc-pal-face nil :weight 'bold))

(defun irc-bitlbee ()
  (interactive)
  (erc-tls :server "testing.bitlbee.org"
           :port 6668
           :nick "langmartin"
           :full-name "Lang Martin"))

(defun bitlbee-identify ()
  (when (and (string= "testing.bitlbee.org" erc-session-server)
	       (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
				     (erc-default-target)
				     bitlbee-password))))

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

(defun irc () (interactive) (irc-freenode))

(set-variables
 '(erc-autoaway-mode t)
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-join-buffer (quote bury))
 '(erc-log-channels-directory "~/.emacs.d/log")
 '(erc-match-mode 1)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-pals (quote ("jlongster" "bweaver")))
 '(erc-server-reconnect-timeout 300)
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-user-full-name "Lang Martin"))

(provide 'irc-rc)
