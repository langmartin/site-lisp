(defmacro prog0 (&rest body) nil)

(defun turn-on-tabs () (interactive) (setq indent-tabs-mode 1))
(defun turn-off-tabs () (interactive) (setq indent-tabs-mode -1))

(defun set-tab-width-2 () (interactive) (setq tab-width 2) (setq c-basic-offset 2))
(defun set-tab-width-4 () (interactive) (setq tab-width 4) (setq c-basic-offset 4))
(defun set-tab-width-8 () (interactive) (setq tab-width 8) (setq c-basic-offset 8))

(defun turn-on-auto-fill () (interactive) (auto-fill-mode 1))
(defun turn-off-auto-fill () (interactive) (auto-fill-mode -1))

(defun turn-on-rainbow-mode () (interactive) (rainbow-mode 1))
(defun turn-off-rainbow-mode () (interactive) (rainbow-mode -1))

(defun turn-down-font-lock () (interactive) (set-variable 'font-lock-maximum-decoration nil 'local))

(defun comment-char-js () (set-variable 'comment-start "//" 'make-local))
(defun comment-char-sh () (set-variable 'comment-start "#" 'make-local))
(defun comment-char-org () (set-variable 'comment-start "#+" 'make-local))

(defun visual-line-not-auto-fill ()
  (interactive)
  (auto-fill-mode -1)
  (visual-line-mode 1))

(defun auto-fill-not-visual-line ()
  (interactive)
  (auto-fill-mode 1)
  (visual-line-mode -1))

(defun turn-on-c-subword-mode ()
  (interactive)
  (if (boundp 'c-subword-mode)
      (c-subword-mode 1)
    (subword-mode 1)))

(defun turn-on-paredit-mode ()
  (interactive)
  (paredit-mode 1))

(defun turn-on-paredit-mode-with-extra-braces ()
  (interactive)
  (turn-on-paredit-mode)
  (local-set-keys
   '(("M-{" . paredit-wrap-curly)
     ("M-}" . paredit-close-curly-and-newline)
     ("M-[" . paredit-wrap-square)
     ("M-]" . paredit-close-square-and-newline))))

(defmacro lambda-insert-with-point (before after)
  `(lambda ()
     (interactive)
     (insert ,before)
     (insert ,after)
     (backward-char (length ,after))))

(provide 'hooks)
