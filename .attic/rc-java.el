;; Java IDE support (JDEE)

(defun set-c-basic-offset-2 () (setq c-basic-offset 2))
(defun set-tab-width-2 () (setq tab-width 2))

(add-hook 'java-mode-hook 'set-c-basic-offset-2)
(add-hook 'java-mode-hook 'set-tab-width-2)

(progn
  (progn
    ;; http://sourceforge.net/projects/cedet/files/
    (add-to-list 'load-path (rc-contrib "cedet-1.0pre6/common/"))
    (add-to-list 'Info-directory-list (rc-contrib "cedet-1.0pre6/"))
    (load "cedet")
    (global-ede-mode 1)
    (semantic-load-enable-code-helpers)
    (global-srecode-minor-mode 1))

  (progn
    ;; http://sourceforge.net/projects/ecb/files/
    (add-to-list 'load-path (rc-contrib "ecb-2.40/"))
    (add-to-list 'Info-directory-list (rc-contrib "ecb-2.40/"))
    (require 'ecb))

  (progn
    ;; http://sourceforge.net/projects/jdee/files/
    (add-to-list 'load-path (rc-contrib "jdee-2.4.0.1/lisp/"))
    (require 'jde)
    (add-to-auto-mode-alist '(("\\.java\\'" . jde-mode)))
    (add-hook 'jde-mode-hook 'c-basic-offset-2)))

(provide 'rc-java)

