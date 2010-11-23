(defmacro prog0 (&rest body) nil)

(defun turn-off-tabs () (interactive) (setq indent-tabs-mode nil))
(defun turn-on-auto-fill () (interactive) (auto-fill-mode 1))
(defun turn-on-rainbow-mode () (interactive) (rainbow-mode t))

(defun comment-char-js () (set-variable 'comment-start "//" 'make-local))
(defun comment-char-sh () (set-variable 'comment-start "#" 'make-local))
(defun comment-char-org () (set-variable 'comment-start "#+" 'make-local))

(defun turn-on-c-subword-mode ()
  (interactive)
  (if (boundp 'c-subword-mode)
      (c-subword-mode 1)
    (subword-mode 1)))

(provide 'hooks)
