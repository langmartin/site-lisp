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
   ("\\.asa$" . html-mode)))

(add-hook 'sgml-mode-hook
	  (lambda ()
            (global-set-keys
             '(("C-c C-p" . sgml-skip-tag-backward)
               ("C-c C-n" . sgml-skip-tag-forward))
             'local)))

(put 'narrow-to-region 'disabled nil)

(provide 'asp-rc)
