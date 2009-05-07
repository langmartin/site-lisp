(defvar compile-site-lisp-sources
  '( ;; anything-config.el
    ;; anything-rc.el
    ;; "anything.el"
    "asp-rc.el"
    "buffer-time-stamp.el"
    "color-theme.el"
    ;; "compile-this-directory.el"
    "csv-nav.el"
    "cx-timesheet.el"
    "gambit.el"
    "gforth.el"
    "git-commands.el"
    "google-define.el"
    "hide-region.el"
    "htmlize.el"
    "http-twiddle.el"
    "irc-rc.el"
    "js2-mode-rc.el"
    "js2-mode.el"
    ;; "lang-dot-emacs.el"
    "lang-mail-rc.el"
    "lang-scripts.el"
    ;; "less.el"
    "moz-rc.el"
    "moz.el"
    "nav.el"
    ;; "org-collector.el"
    "paredit.el"
    "php-mode.el"
    "rst.el"
    "scheme-indent.el"
    "scheme48.el"
    "session.el"
    "site-start.el"
    "smooth-scrolling.el"
    "srfi-1.el"
    "term-mode-rc.el"
    "textmate.el"
    "timeclock-rc.el"
    "visual-basic-mode.el"
    ;; "windows-backup.el"
    ))

(defun compile-site-lisp ()
  (interactive)
  (mapc 'byte-compile-file compile-site-lisp-sources))

(defun delete-site-lisp-byte-code ()
  (mapc (lambda (file)
         (delete-file (concat file "c")))
       site-lisp-sources))

(provide 'compile-site-lisp)
