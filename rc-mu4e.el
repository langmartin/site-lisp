(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)

(defun mu4e-and-update ()
  (interactive)
  (mu4e)
  (mu4e-update-mail-and-index nil))

(global-set-key
 (kbd "C-x m")
 (alist-to-keymap-via-kbd
  '(("m" . mu4e-compose-new)
    ("r" . mu4e))))

;; brew install --with-emacs mu
;; brew install offlineimap
;; brew install html2text

(custom-set-variables
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-date-format-long "%Y-%m-%d")
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-headers-date-format "%y-%m-%d")
 '(mu4e-headers-fields (quote ((:human-date . 12) (:flags . 6) (:from . 22) (:subject) (:maildir))))
 '(mu4e-html2text-command "html2text -width 72 -nobs -utf8")
 '(mu4e-maildir-shortcuts (quote (("/INBOX" . 105) ("/Archive" . 97))))
 '(mu4e-bookmarks (quote (("flag:unread AND NOT flag:trashed" "Unread messages" 117) ("flag:flagged" "Flagged" 43) ("date:today..now" "Today's messages" 116) ("date:7d..now" "Last 7 days" 119) ("mime:image/*" "Messages with images" 112))))
 '(mu4e-refile-folder "/Archive")
 '(mu4e-sent-folder "/Archive")
 '(mu4e-trash-folder "/Deleted Items"))

(provide 'rc-mu4e)
