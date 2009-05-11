(require 'erc)

(set-variables
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-match-mode 1)
 '(erc-autoaway-mode t)
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-log-channels-directory "~/.emacs.d/log")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#jquery" "#scheme"))))
 )

(defun irc-bitlbee ()
  (interactive)
  (erc-tls :server "testing.bitlbee.org"
           :port 8001
           :nick "langmartin"
           :full-name "Lang Martin"))

;; (defun bitlbee-identify ()
;;   (when (and (string= "testing.bitlbee.org" erc-session-server)
;; 	       (string= "&bitlbee" (buffer-name)))
;;     (erc-message "PRIVMSG" (format "%s identify %s"
;; 				     (erc-default-target)
;; 				     "<password>"))))
;; (custom-set-variables
;;  '(erc-join-hook (quote (bitlbee-identify))))

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
       :password freenode-password))

;; (define-buffer-visitor visit-medium "#medium" 'irc)
;; (global-set-key (kbd "H-m") 'visit-medium)

(defun irc () (interactive) (irc-freenode))

(provide 'irc-rc)
