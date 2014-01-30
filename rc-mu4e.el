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
    ("r" . mu4e)
    ("u" . mu4e-and-update))))

;; brew install --with-emacs mu
;; brew install offlineimap
;; brew install html2text

(defun mu4e-view-with-html2text ()
  (interactive)
  (let ((buf (get-buffer-create mu4e-output-buffer-name))
        (msg (mu4e-message-at-point)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (mu4e-message-field msg :body-html))
        (call-process-region (point-min) (point-max)
                             "html2text"
                             t buf t
                             "-width" "72"
                             "-nobs"
                             "-utf8")
        (view-mode)
        (switch-to-buffer buf)))))

(define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text)

(custom-set-variables
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-date-format-long "%Y-%m-%d")
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-headers-date-format "%y-%m-%d")
 '(mu4e-headers-fields (quote ((:human-date . 12) (:flags . 6) (:from . 22) (:subject))))
 '(mu4e-headers-leave-behavior (quote apply))
 '(mu4e-view-show-addresses t)
 '(mu4e-html2text-command
   ;; "html2text -width 72 -nobs -utf8"
   nil)
 '(mu4e-bookmarks (quote (("flag:unread AND NOT flag:trashed" "Unread messages" 117) ("flag:flagged OR maildir:/INBOX" "Flagged" 105) ("date:today..now" "Today's messages" 116) ("date:7d..now" "Last 7 days" 119) ("mime:image/*" "Messages with images" 112))))
 '(mu4e-confirm-quit nil)
 '(mu4e-refile-folder "/Archive")
 '(mu4e-sent-folder "/Archive")
 '(mu4e-trash-folder "/Deleted Items"))

(provide 'rc-mu4e)
