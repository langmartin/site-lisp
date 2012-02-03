(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'maxframe nil t)
(require 'org-compat nil t)

(when (package-installed-p 'project-mode)
  (require 'project-mode)
  (project-load-all))

(when (package-installed-p 'session)
  (session-initialize))

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
          project-mode
          rcirc
          session
          ;; Programming modes
          ;; js2-mode
          clojure-mode
          org-compat
          )))

(provide 'rc-package)
