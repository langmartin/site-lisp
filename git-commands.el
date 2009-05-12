(defmacro define-shellcmd (name cmd)
  `(defun ,name ()
     (interactive)
     (shell-command ,cmd)))

(define-shellcmd git-status "git status")
(define-shellcmd git-pull "git pull -q")
(define-shellcmd git-fetch "git fetch")

(defun git-merge (branch)
  (interactive "sBranch: ")
  (shell-command (concat "git merge -q " branch)))

(defun git-push (extra)
  (interactive "sPush: ")
  (shell-command (concat "git push " extra)))

(define-shellcmd git-branches "git branch -av")

(defun git-checkout (branch)
  (interactive "sBranch: ")
  (shell-command (concat "git checkout " branch)))

(defun git-grep (command)
  "Run git-grep like grep"
  (interactive
   (list
    (read-from-minibuffer
     "Run git-grep (like this): "
     "git-grep -n -H -e")))
  (grep command))

;; (defun lines-to-list ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((acc '()))
;;       (while (not (eobp))
;;         (search-forward-regexp "^[:space:]*\\(.*\\)[:space:]*$" nil t)
;;         (setq acc (cons (match-string 1)
;;                         acc)))
;;       acc)))

;; (defun git-grep-dired (command)
;;   (interactive
;;    (list
;;     (read-from-minibuffer
;;      "Run git-grep (like this): "
;;      "git-grep -n -H -e")))
;;   (with-temp-buffer
;;     (let ((coding-system-for-read 'dos))
;;      (shell-command command (current-buffer))
;;      (dired ))))

(define-key ctl-x-map "g"
  (easy-mmode-define-keymap
   '(("s" . git-status)
     ("f" . git-fetch)
     ("p" . git-pull)
     ("P" . git-push)
     ("m" . git-merge)
     ("b" . git-branches)
     ("c" . git-checkout))))

(provide 'git-commands)
