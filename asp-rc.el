(require 'visual-basic-mode)
(setq visual-basic-mode-indent 4)
(add-hook 'visual-basic-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(defun rc-sgml-mode-for-asp ()
  (interactive)
  (widen)
  (html-mode))

(add-to-auto-mode-alist
 '(("\\.asp$" . visual-basic-mode)
   ("\\.asa$" . visual-basic-mode)))

(add-hook 'sgml-mode-hook
	  (lambda ()
            (local-set-keys
             '(("C-c C-p" . sgml-skip-tag-backward)
               ("C-c C-n" . sgml-skip-tag-forward)))))

(put 'narrow-to-region 'disabled nil)

(provide 'asp-rc)
