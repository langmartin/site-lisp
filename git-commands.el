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
  (define-shellcmd git-fetch "git fetch")
  (define-shellcmd git-log "git log --graph"))

(defun git-merge (branch)
  (interactive
   (list
    (read-from-minibuffer
     "git merge "
     "origin/master")))
  (shell-command (concat "git merge -q " branch)))

(defun git-push (extra)
  (interactive
   (list
    (read-from-minibuffer
     "git push "
     "origin")))
  (shell-command (concat "git push " extra)))

(define-shellcmd git-branches "git branch -av")

(defun git-checkout (branch)
  (interactive
   (list
    (read-from-minibuffer
     "git checkout "
     "master")))
  (shell-command (concat "git checkout " branch)))

(defun git-grep (command)
  "Run git-grep like grep"
  (interactive
   (list
    (read-from-minibuffer
     "Run git-grep (like this): "
     "git-grep -n -H -I -e ")))
  (grep (concat command " .")))

(defun lines-to-list ()
  (let ((body (buffer-substring-no-properties (point-min) (point-max))))
    (let ((acc '()) (idx 0) (len (length body)))
      (while (< idx len)
        (setq idx (+ (string-match "^[[:space:]]*\\(.*?\\)[[:space:]]*$"
                                   body
                                   idx)
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
    (shell-command (concat command " .") (current-buffer))
    (dired (cons "*git-grep-dired*"
                 (lines-to-list)))))

(defun git-add ()
  "Add the current file to the index"
  (interactive)
  (shell-command (concat "git add \"" (buffer-file-name) "\"")))

(defun git-commit ()
  (interactive)
  "Commit"
  (shell-command "git commit &"))

(defun git-diff (cachedp)
  "Run git diff on the current file or directory. With the prefix argument, run git diff --cached."
  (interactive "P")
  (let* ((cmd (if cachedp "git diff --cached ."
                "git diff ."))
         (bufname (concat "*" cmd "*")))
    (message cmd bufname)
    (shell-command cmd bufname)
    (with-buffer (get-buffer bufname)
      (diff-mode))))

(defvar git-commands-map)

(setq git-commands-map
      (easy-mmode-define-keymap
       '(("\C-xga" . git-add)
         ("\C-xgb" . git-branches)
         ("\C-xgB" . git-checkout)
         ("\C-xgc" . git-commit)
         ("\C-xgd" . git-diff)
         ("\C-xgf" . git-fetch)
         ("\C-xgg" . git-grep)
         ("\C-xgG" . git-grep-dired)
         ("\C-xgl" . git-log)
         ("\C-xgm" . git-merge)
         ("\C-xgP" . git-push)
         ("\C-xgs" . git-status))))

(define-minor-mode git-commands-mode
  "Some git commands bound to C-x g * for operating in the current directory."
  t
  nil
  git-commands-map)

(provide 'git-commands)
