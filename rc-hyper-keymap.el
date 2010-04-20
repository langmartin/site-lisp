(global-set-key (kbd "H-i") 'imenu)
(global-set-key (kbd "H-r") 'repeat)
(global-set-key (kbd "H-h") 'help)

(defun rc-windmove-keybindings (&optional modifier)
  (interactive)
  (unless modifier (setq modifier '(hyper)))
  (global-set-key (vector (append modifier '(left)))  'windmove-left)
  (global-set-key (vector (append modifier '(right))) 'windmove-right)
  (global-set-key (vector (append modifier '(up)))    'windmove-up)
  (global-set-key (vector (append modifier '(down)))  'windmove-down))

(rc-windmove-keybindings)

(progn
  (defmacro lambda-insert-with-point (before after)
    `(lambda ()
       (interactive)
       (insert ,before)
       (insert ,after)
       (backward-char (length ,after))))
  
  (define-key (current-global-map) (kbd "H-l")
    (lambda-insert-with-point "(lambda ()" ")"))

  (define-key js2-mode-map (kbd "H-l")
    (lambda-insert-with-point "function () {" "};")))

(provide 'rc-hyper-keymap)
