(require 'js2-mode)

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 )

(add-to-auto-mode-alist
 '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(provide 'js2-mode-rc)
