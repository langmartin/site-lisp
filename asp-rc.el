(require 'visual-basic-mode)

(defun rc-sgml-mode-for-asp ()
  (interactive)
  (widen)
  (html-mode))

(add-to-auto-mode-alist
 '(("\\.asp$" . html-mode)
   ("\\.asa$" . html-mode)))

(add-hook 'sgml-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-p" 'sgml-skip-tag-backward)
	    (local-set-key "\C-c\C-n" 'sgml-skip-tag-forward)))

(put 'narrow-to-region 'disabled nil)

(provide 'asp-rc)
