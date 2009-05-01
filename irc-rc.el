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

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
;; (add-hook 'erc-text-matched-hook 'erc-growl-match)
(erc-match-mode 1)

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
