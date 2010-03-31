(global-set-key (kbd "H-i") 'imenu)
(global-set-key (kbd "H-r") 'repeat)
(global-set-key (kbd "H-h") 'help)

(defun rc-windmove-keybindings (&optional modifier)
  "Set up keybindings for `windmove'.
Keybindings are of the form MODIFIER-{left,right,up,down}.
Default MODIFIER is 'hyper."
  (interactive)
  (unless modifier (setq modifier '(hyper)))
  (global-set-key (vector (append modifier '(left)))  'windmove-left)
  (global-set-key (vector (append modifier '(right))) 'windmove-right)
  (global-set-key (vector (append modifier '(up)))    'windmove-up)
  (global-set-key (vector (append modifier '(down)))  'windmove-down))

(rc-windmove-keybindings)

(progn
  (global-set-key (kbd "H-l") "λ")
  (global-set-key (kbd "H-l") "lambda")
  (define-key js2-mode-map (kbd "H-l") "function () { }"))

(provide 'rc-hyper-keymap)
