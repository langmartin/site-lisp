(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defmacro with-feature (feature &rest body)
  "If a feature can be required, run the initialization code.
Otherwise warn."
  (declare (indent 1))
  `(if (require ',feature nil t)
       (progn ,@body)
     (warn "feature %s is not installed" ',feature)))

(with-feature maxframe)
(with-feature org-compat)

(with-feature project-mode
  (project-load-all))

(with-feature session
  (session-initialize))

(with-feature magit
  (add-to-info-path "~/.emacs.d/elpa/magit-1.1.1/"))

(with-feature org
  (add-to-info-path "~/.emacs.d/elpa/org-20120207/"))

;; (with-feature sws-mode
;;   (with-feature jade-mode
;;     (add-hook 'jade-mode-hook 'turn-off-tabs)))

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
          zencoding
          )))

(provide 'rc-package)
