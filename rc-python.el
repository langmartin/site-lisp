;;; Python

(add-hook 'python-mode-hook 'customize-python-mode)

(defun customize-python-mode ()
  (define-key python-mode-map "\C-x\C-s" 'cleanup-untabify-save)
  (define-key python-mode-map [(return)] 'newline-and-indent))

(defun cleanup-buffer ()
  (interactive)
  (delete-trailing-whitespace))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-untabify-save ()
  (interactive)
  (cleanup-buffer)
  (untabify-buffer)
  (save-buffer))

(provide 'rc-python)
