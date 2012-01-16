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

(global-set-key (kbd "H-s") 'eshell-focus-or-create)

(provide 'rc-eshell)
