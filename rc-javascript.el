(require 'js2-mode)

;;; if you just set these, they're buffer local. If you stick them in
;;; the hook, they're not overridable (easily). So, we do a second
;;; customize block.

(add-to-auto-mode-alist
 '(("\\.js\\'" . js2-mode)))

(set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-mirror-mode nil))

(defun turn-on-c-subword-mode ()
  (interactive)
  (c-subword-mode 1))

(add-hook 'js2-mode-hook 'turn-off-indent-tabs-mode)
(add-hook 'js2-mode-hook 'turn-on-c-subword-mode)
(provide 'js2-mode-rc)


