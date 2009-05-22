(defmacro define-shellcmd (name cmd)
  `(defun ,name ()
     (interactive)
     (shell-command ,cmd)))

(define-shellcmd git-status "git status")
(define-shellcmd git-pull "git pull -q")
(define-shellcmd git-fetch "git fetch")
(define-shellcmd git-log "git log --graph")

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

(define-key ctl-x-map "g"
  (easy-mmode-define-keymap
   '(("b" . git-branches)
     ("c" . git-checkout)
     ("f" . git-fetch)
     ("G" . git-grep-dired)
     ("g" . git-grep)
     ("l" . git-log)
     ("m" . git-merge)
     ("p" . git-pull)
     ("P" . git-push)
     ("s" . git-status))))

(provide 'git-commands)
