(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)

(defun mu4e-and-update ()
  (interactive)
  (mu4e)
  (mu4e-update-mail-and-index nil))

(global-set-key
 (kbd "C-x m")
 (alist-to-keymap-via-kbd
  '(("c" . mu4e-compose-new)
    ("m" . mu4e-compose-new)
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

(defun rc-mu4e-starred ()
  (defvar mu4e-starred-folder "/starred")
  (defun mu4e-headers-mark-move-to-starred ()
    (interactive)
    (mu4e-mark-set 'move mu4e-starred-folder)
    (mu4e-headers-next))
  (defun mu4e-view-mark-move-to-starred ()
    (interactive)
    (mu4e~view-in-headers-context
     (mu4e-headers-mark-move-to-starred))))

(defun rc-mu4e-junk-mail ()
  (defvar mu4e-junk-folder "/Junk Email")
  (defun mu4e-headers-mark-move-to-junk ()
    (interactive)
    (mu4e-mark-set 'move mu4e-junk-folder)
    (mu4e-headers-next))
  (defun mu4e-view-mark-move-to-junk ()
    (interactive)
    (mu4e~view-in-headers-context
     (mu4e-headers-mark-move-to-junk))))

(defun rc-mu4e-gmail-shortcuts ()
  (define-key mu4e-main-mode-map "g" 'mu4e-headers-search-bookmark)
  (define-key mu4e-main-mode-map "/" 'mu4e-headers-search)
  (define-key mu4e-headers-mode-map "G" 'mu4e-headers-rerun-search)
  (define-key mu4e-headers-mode-map "g" 'mu4e-headers-search-bookmark)
  (define-key mu4e-view-mode-map "G" 'mu4e-view-go-to-url)
  (define-key mu4e-view-mode-map "g" 'mu4e-headers-search-bookmark)

  (define-key mu4e-headers-mode-map "y" 'mu4e-headers-mark-for-refile)
  (define-key mu4e-view-mode-map "y" 'mu4e-view-mark-for-refile)
  (define-key mu4e-headers-mode-map "s" 'mu4e-headers-mark-move-to-starred)
  (define-key mu4e-view-mode-map "s" 'mu4e-view-mark-move-to-starred)
  (define-key mu4e-headers-mode-map "!" 'mu4e-headers-mark-move-to-junk)
  (define-key mu4e-view-mode-map "!" 'mu4e-view-mark-move-to-junk)
  (define-key mu4e-headers-mode-map "#" 'mu4e-headers-mark-for-trash)
  (define-key mu4e-view-mode-map "#" 'mu4e-view-mark-for-trash)

  (setq
   mu4e-bookmarks
   `((,(concat "(maildir:/INBOX OR flag:unread)"
               " AND NOT ("
               "maildir:" mu4e-trash-folder
               " OR "
               "maildir:" mu4e-junk-folder ")") "Unread" 105)
     (,(concat "flag:flagged OR maildir:" mu4e-starred-folder) "Starred" 115)
     (,(concat "from:" user-mail-address " AND date:30d..now") "Last 30 days sent" 116)
     (,(concat "flag:draft OR maildir:" mu4e-drafts-folder) "Drafts" 100)
     ("date:7d..now" "Last 7 days" 97))))

(rc-mu4e-starred)
(rc-mu4e-junk-mail)
(rc-mu4e-gmail-shortcuts)

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
  '(mu4e-junk-folder "/[Gmail].Trash")
  '(mu4e-headers-skip-duplicates t)
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
 '(mu4e-confirm-quit nil))

;; (custom-save-all)

(provide 'rc-mu4e)
