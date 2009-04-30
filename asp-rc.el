(require 'visual-basic-mode)

(defun rc-sgml-mode-for-asp ()
  (interactive)
  (widen)
  (html-mode))

(setq auto-mode-alist
      (cons '("\\.asp$" . html-mode)
	    auto-mode-alist))

(add-to-auto-mode-alist
 '("\\.asp$" . html-mode)
 '("\\.asa$" . html-mode))

(add-hook 'sgml-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-p" 'sgml-skip-tag-backward)
	    (local-set-key "\C-c\C-n" 'sgml-skip-tag-forward)))

(put 'narrow-to-region 'disabled nil)

(progn
  (global-set-key [M-f4] 'delete-frame)
  (global-set-key [f5] 'eshell)
  (global-set-key [f6] 'imenu)
  (global-set-key [f7] 'normal-mode)
  (global-set-key [f8] 'toggle-truncate-lines)

  (global-set-key [f10] 'visual-basic-mode)
  (global-set-key [f9] 'css-mode)
  (global-set-key [f11] 'js2-mode)
  (global-set-key [f12] 'rc-sgml-mode-for-asp))

(provide 'asp-rc)
