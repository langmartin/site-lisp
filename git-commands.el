(defmacro define-shellcmd (name cmd)
  `(defun ,name ()
     (interactive)
     (shell-command ,cmd)))

(define-shellcmd git-status "git status")

(define-shellcmd git-pull "git pull -q")

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

(define-key ctl-x-map "g"
  (easy-mmode-define-keymap
   '(("s" . git-status)
     ("p" . git-pull)
     ("m" . git-merge)
     ("b" . git-branches)
     ("c" . git-checkout))))
