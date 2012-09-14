(custom-set-variables
 '(eshell-ls-use-colors nil)
 '(eshell-prompt-function (lambda nil (concat (number-to-string eshell-last-command-status) " " (eshell/pwd) (if (= (user-uid) 0) " # " " $ "))))
 '(eshell-visual-commands (quote ("ssh" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"))))

(defun eshell-focus-or-create (prefix)
  (interactive "P")
  (cond (prefix
         (eshell prefix))
        ((bufferp (get-buffer "*eshell*"))
         (let ((dir default-directory))
           (switch-to-buffer "*eshell*")
           (unless (equal dir default-directory)
             (cd dir)
             (eshell-send-input))))
        (t
         (eshell nil))))

;; (global-set-key (kbd "H-s") 'eshell-focus-or-create)

(defun shell-focus-or-create (prefix)
  (interactive "P")
  (cond (prefix
         (call-interactively #'shell))
        ((bufferp (get-buffer "*shell*"))
         (let ((dir default-directory))
           (switch-to-buffer "*shell*")
           (unless (equal dir default-directory)
             (insert "cd " dir)
             (comint-send-input))))
        (t
         (shell nil))))

(defun send-c-c-interrupt-subjob (prefix)
  (interactive "P")
  (cond (prefix
         (comint-interrupt-subjob))
        (t
         (insert "")
         (comint-send-input))))

(define-key shell-mode-map (kbd "C-c C-c") 'send-c-c-interrupt-subjob)

(global-set-key (kbd "H-s") 'shell-focus-or-create)

(provide 'rc-eshell)
