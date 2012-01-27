(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'maxframe nil t)
(require 'org-compat nil t)

(if (package-installed-p 'project-mode)
    (project-load-all))

(defun rc-package-install-packages ()
  "Install initial packages"
  (interactive)
  (mapc (lambda (p)
          (if (not (package-installed-p p))
              (package-install p)))
        '(
          ;; Utilities
          highlight-symbol
          htmlize
          guess-offset
          jabber
          magit
          maxframe
          rcirc
          ;; Programming modes
          ;; js2-mode
          clojure-mode
          org-compat
          )))

(provide 'rc-package)
