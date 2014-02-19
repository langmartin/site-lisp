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

;;;; Toggle viewing with the other html viewer

(defun mu4e-view-with-html2text-on ()
  (interactive)
  (let ((mu4e-html2text-command
         (if mu4e-html2text-command
             nil
           "html2text -width 72 -nobs -utf8")))
    (define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text-off)
    (mu4e-view-refresh)))

(defun mu4e-view-with-html2text-off ()
  (interactive)
  (define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text-on)
  (mu4e-view-refresh))

(define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text-on)

;;;; Flag by moving to a special folder; flags don't sync well to
;;;; exchange

(defun mu4e-headers-mark-move-to-follow ()
  (interactive)
  (mu4e-mark-set 'move "/follow")
  (mu4e-headers-next))

(defun mu4e-view-mark-move-to-follow ()
  (interactive)
  (mu4e~view-in-headers-context
   (mu4e-headers-mark-move-to-follow)))

(define-key mu4e-headers-mode-map "!" 'mu4e-headers-mark-move-to-follow)
(define-key mu4e-view-mode-map "!" 'mu4e-view-mark-move-to-follow)

;; (setq mu4e-get-mail-command "offlineimap")
;; (setq mu4e-get-mail-command "true")

(prog0
 (custom-set-variables
  '(mu4e-refile-folder "/Archive")
  '(mu4e-sent-folder "/Archive")
  '(mu4e-trash-folder "/Deleted Items")
  '(mu4e-drafts-folder "/Drafts"))

 (custom-set-variables
  '(mu4e-refile-folder "/[Gmail].All Mail")
  '(mu4e-sent-folder "/[Gmail].All Mail")
  '(mu4e-trash-folder "/[Gmail].Trash")
  '(mu4e-drafts-folder "/[Gmail].Drafts")))

(custom-set-variables
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-date-format-long "%Y-%m-%d")
 '(mu4e-headers-date-format "%y-%m-%d")
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-headers-fields (quote ((:human-date . 12) (:flags . 6) (:from . 22) (:subject))))
 '(mu4e-headers-leave-behavior (quote apply))
 '(mu4e-html2text-command "html2text -width 72 -nobs -utf8")
 '(mu4e-view-show-addresses t)
 '(mu4e-bookmarks (quote (("flag:unread AND NOT flag:trashed OR maildir:/INBOX" "Unread messages" 117) ("flag:flagged OR maildir:/follow" "Flagged" 105) ("date:today..now" "Today's messages" 116) ("date:7d..now" "Last 7 days" 119) ("from:lang AND date:7d..now" "Last week from me" 115) ("flag:draft OR maildir:/Drafts" "Drafts" 100))))
 '(mu4e-confirm-quit nil))

;; (custom-save-all)

(provide 'rc-mu4e)
