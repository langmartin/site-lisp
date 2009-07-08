(require 'js2-mode)

;;; if you just set these, they're buffer local. If you stick them in
;;; the hook, they're not overridable (easily). So, we do a second
;;; customize block.

(add-to-auto-mode-alist
 '(("\\.js\\'" . js2-mode)))

(defun show-paren-style-expression ()
  (make-local-variable 'show-paren-style)
  (setq show-paren-style 'expression))

(add-hook 'js2-mode-hook 'turn-off-indent-tabs-mode)
(add-hook 'js2-mode-hook 'show-paren-style-expression)

(provide 'js2-mode-rc)
