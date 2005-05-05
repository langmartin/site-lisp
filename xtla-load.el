; -*- mode: emacs-lisp -*-
;;
;; Load Xtla easily ...
;;
;; Manually, you can run
;;
;;   M-x load-file RET /path/to/xtla-load.el RET
;;
;; (usefull when you want to load Xtla after starting "emacs -q"!), or
;; add
;;
;;   (load-file "/path/to/this/file/in/installdir/xtla-load.el")
;;
;; to your ~/.emacs.el

(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/contrib")
(add-to-list 'Info-default-directory-list "/usr/share/info/info")

(if (featurep 'xtla)
    (tla-reload)
  (if (featurep 'xemacs)
      (require 'auto-autoloads)
    (require 'xtla-autoloads)))

; arch-tag: Matthieu Moy, Thu Mar 10 23:23:46 2005 (xtla-load-install.el.in)
