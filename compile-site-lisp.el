;; -*- no-byte-compile: t -*-
(defun compile-site-lisp-directory ()
  (file-name-directory
   (find-lisp-object-file-name 'compile-site-lisp 'function)))

(defun compile-site-lisp-sources (&optional dir tail)
  (fold (lambda (x acc)
          (if (and (file-directory-p x) (member x load-path))
              (compile-site-lisp-sources x acc)
            (if (and (string-match "\\.el$" x)
                     (not (or (equal x "compile-site-lisp.el")
                              (string-match "-rc\\.el$" x)
                              (string-match "/rc-" x))))
                (cons x acc)
              acc)))
        tail
        (directory-files (or dir (compile-site-lisp-directory)) t)))

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
    (cd (compile-site-lisp-directory))
    (mapc (lambda (file)
            (if (> (file-mtime file)
                   (file-mtime (concat file "c")))
                (byte-compile-file file)))
          (compile-site-lisp-sources))))

(provide 'compile-site-lisp)
