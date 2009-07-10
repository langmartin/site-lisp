(require 'visual-basic-mode)
(setq visual-basic-mode-indent 4)
(add-hook 'visual-basic-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(defun rc-sgml-mode-for-asp ()
  (interactive)
  (widen)
  (html-mode))

(add-to-auto-mode-alist
 '(("\\.asp$" . html-mode)
   ("\\.asa$" . visual-basic-mode)))

(add-hook 'sgml-mode-hook
	  (lambda ()
            (local-set-keys
             '(("C-c C-p" . sgml-skip-tag-backward)
               ("C-c C-n" . sgml-skip-tag-forward)))))

(defun enable-truncate-long-lines ()
  (toggle-truncate-lines 1))

(add-hook 'html-mode-hook 'enable-truncate-long-lines)

(put 'narrow-to-region 'disabled nil)

(provide 'asp-rc)
