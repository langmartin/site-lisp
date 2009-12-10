;; -*- no-byte-compile: t -*-
(setq compile-site-lisp-sources
      '("buffer-time-stamp.el"
        "color-theme.el"
        "csv-nav.el"
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
        "php-mode.el"
        "rst.el"
        "scheme/gambit.el"
        "scheme/paredit.el"
        "scheme/scheme-indent.el"
        "scheme/scheme48.el"
        "session.el"
        "site-start.el"
        "smooth-scrolling.el"
        "utility.el"
        "visual-basic-mode.el"
        "chop.el"
        "make-tags-file.el"
        ))

(defun file-mtime (file)
  (if (file-exists-p file)
      (float-time
       (nth 5 (file-attributes file)))
    0))

(defun compile-site-lisp ()
  "compile a configured list of .el files in the user site-lisp
directory. See site-lisp-directory and
compile-site-lisp-sources."
  (interactive)
  (with-temp-buffer
    (cd site-lisp-directory)
    (mapc (lambda (file)
            (if (> (file-mtime file)
                   (file-mtime (concat file "c")))
                (byte-compile-file file)))
          compile-site-lisp-sources)))

;;; 1) See the first line of this file; it's a good idea to mark files
;;;    with this modeline that shouldn't be compiled.
;;;
;;; 2) Use C-x M-e to evaluate the functions in place. That binding is
;;;    defined in site-lisp. The functions below will insert valid
;;;    list fragments, which makes updating the list more convenient.

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
