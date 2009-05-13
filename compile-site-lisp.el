(setq site-lisp-directory "~/site-lisp")

(setq compile-site-lisp-sources
      '("anything-config.el"
        "anything.el"
        "buffer-time-stamp.el"
        "color-theme.el"
        "csv-nav.el"
        "gambit.el"
        "gforth.el"
        "git-commands.el"
        "google-define.el"
        "hide-region.el"
        "htmlize.el"
        "http-twiddle.el"
        "js2-mode.el"
        ;; "lang-dot-emacs.el"
        "lang-scripts.el"
        "moz.el"
        "nav.el"
        "org-collector.el"
        "paredit.el"
        "php-mode.el"
        "rst.el"
        "scheme-indent.el"
        "scheme48.el"
        "session.el"
        "site-start.el"
        "smooth-scrolling.el"
        "srfi-1.el"
        "textmate.el"
        "utility.el"
        "visual-basic-mode.el"
        ))

(defun file-mtime (file)
  (if (file-exists-p file)
      (float-time
       (nth 5 (file-attributes file)))
    0))

(defun compile-site-lisp ()
  (interactive)
  (mapc (lambda (file)
          (if (> (file-mtime file)
                 (file-mtime (concat file "c")))
              (byte-compile-file file)))
        compile-site-lisp-sources))

(defun site-lisp-missing-files ()
  (fold (lambda (x acc)
          (if (or (equal x "compile-site-lisp.el")
                  (member x compile-site-lisp-sources)
                  (string-match "-rc\\.el$" x))
              acc
            (cons x
                  acc)))
        '()
        (directory-files site-lisp-directory nil "\\.el$")))

;; (site-lisp-missing-files)

(defun site-lisp-extra-files ()
  (fold (lambda (x acc)
          (if (not (file-exists-p (concat site-lisp-directory "/" x)))
              (cons x acc)
            acc))
        '()
        compile-site-lisp-sources))

;; (site-lisp-extra-files)

(provide 'compile-site-lisp)
