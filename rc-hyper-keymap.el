(global-set-keys
 (mapcar (lambda (b) (cons (concat "H-" (car b)) (cdr b)))
         `(("]" . vi-mode)
           ;; ("h" . help)
           ("i" . imenu)
           ("R" . repeat)
           ("r" . revert-buffer)
           ("t" . toggle-truncate-lines))))

(global-set-keys
 `(("C-x C-j" . execute-extended-command)
   ("C-c C-j" . execute-extended-command)
   ("M-<f4>" . delete-frame)
   ))

(defun rc-windmove-keybindings (&optional modifier)
  (interactive)
  (unless modifier (setq modifier '(hyper)))
  (global-set-key (vector (append modifier '(left)))  'windmove-left)
  (global-set-key (vector (append modifier '(right))) 'windmove-right)
  (global-set-key (vector (append modifier '(up)))    'windmove-up)
  (global-set-key (vector (append modifier '(down)))  'windmove-down))

(rc-windmove-keybindings)

(progn
  (define-key (current-global-map) (kbd "H-l")
    (lambda-insert-with-point "(lambda ()" ")"))
  )

(provide 'rc-hyper-keymap)
