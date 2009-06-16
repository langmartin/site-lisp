(defmacro define-shellcmd (name cmd &optional hook)
  `(defun ,name ()
     (interactive)
     (let ((bufname (concat "*" ,cmd "*")))
       (shell-command ,cmd bufname)
       (if ,hook
           (with-buffer (get-buffer bufname)
             (funcall ,hook))))))

(progn
 (define-shellcmd git-status "git status")
 (define-shellcmd git-pull "git pull -q")
 (define-shellcmd git-fetch "git fetch")
 (define-shellcmd git-log "git log --graph")
 (define-shellcmd git-diff "git diff ." 'diff-mode))

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
     "git-grep -n -H -I -e ")))
  (grep command))

(defun lines-to-list ()
  (let ((body (buffer-substring-no-properties (point-min) (point-max))))
    (let ((acc '()) (idx 0) (len (length body)))
      (while (< idx len)
        (setq idx (+ (string-match "^[[:space:]]*\\(.*?\\)[[:space:]]*$" body idx)
                     1))
        (let ((got (match-string 1 body)))
          (if (> (length got) 0)
              (setq acc (cons got acc)))))
      acc)))

(defun git-grep-dired (command)
  (interactive
   (list
    (read-from-minibuffer
     "Run git-grep for dired (like this): "
     "git-grep -l -e ")))
  (with-temp-buffer
    (shell-command command (current-buffer))
    (dired (cons "*git-grep-dired*"
                 (lines-to-list)))))

(defvar git-commands-map)

(setq git-commands-map
      (easy-mmode-define-keymap
       '(("\C-xgb" . git-branches)
         ("\C-xgc" . git-checkout)
         ("\C-xgd" . git-diff)
         ("\C-xgf" . git-fetch)
         ("\C-xgG" . git-grep-dired)
         ("\C-xgg" . git-grep)
         ("\C-xgl" . git-log)
         ("\C-xgm" . git-merge)
         ("\C-xgp" . git-pull)
         ("\C-xgP" . git-push)
         ("\C-xgs" . git-status))))

(define-minor-mode git-commands-mode
  "Some git commands bound to C-x g * for operating in the current directory."
  t
  nil
  git-commands-map)

(provide 'git-commands)
