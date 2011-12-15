(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; These require forms don't get called by package-initialize the way
;;; you'd expect.
(require 'maxframe)
(require 'org-compat)

(defun rc-package-install-packages ()
  "Install initial packages"
  (interactive)
  (mapc (lambda (p)
          (if (not (package-installed-p p))
              (package-install p)))
        '(
          ;; Utilities
          htmlize
          guess-offset
          magit
          ;; Programming modes
          ;; js2-mode
          clojure-mode
          )))

(provide 'rc-package)
