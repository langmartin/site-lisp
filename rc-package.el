(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;; pulls from git master branch
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (unless (package-installed-p 'bbdb) (package-install 'bbdb))

(defmacro with-feature (feature &rest body)
  "If a feature can be required, run the initialization code.
Otherwise warn."
  (declare (indent 1))
  `(if (require ',feature nil t)
       (progn ,@body)
     (warn "feature %s is not installed" ',feature)))

(defun rc-package-install-packages ()
  "Install initial packages"
  (interactive)
  (mapc (lambda (p)
          (if (not (package-installed-p p))
              (package-install p)))
        '(
          highlight-symbol
          htmlize
          guess-offset
          magit
          find-file-in-repository
          ;; Programming modes
          paredit
          geiser
          js2-mode
          cider
          clojure-mode
          clojure-test-mode
          zencoding-mode
          )))

(rc-package-install-packages)

(provide 'rc-package)
