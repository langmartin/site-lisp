(defun join (list splice)
  "Join a sequence of strings with the string splice."
  (apply 'concat
         (cons (car list)
               (fold (lambda (x acc)
                       (if (null x) acc
                         (cons splice
                               (cons x acc))))
                     nil
                     (cdr list)))))

(assert
 (equal "foo" (join '("foo") "/"))
 (equal "foo/bar" (join '("foo" "bar") "/")))

(defmacro define-shell-command (name cmd &optional default hook sync)
  `(defun ,name (&optional prefix)
     (interactive "P")
     (let ((command ,cmd)
           (bufname (concat "*" ,cmd "*"))
           (dir default-directory))
       (if (or prefix ,default)
           (setq command
                 (read-from-minibuffer
                  "Shell command: "
                  ,(join (list cmd default) " "))))
       (if ,sync
           (shell-command command bufname)
         (shell-command (concat command "&") bufname))
       (with-current-buffer (get-buffer bufname)
         (progn
           (cd dir)
           (if ,hook (funcall ,hook)))))))

(progn
  (define-shell-command git-status "git status")
  (define-shell-command git-fetch "git fetch")
  (define-shell-command git-log "git log --graph")
  (define-shell-command git-branches "git branch -av")
  (define-shell-command git-commit "git commit" "--amend")
  (define-shell-command git-merge "git merge -q" "origin/master")
  (define-shell-command git-pull "git pull")
  (define-shell-command git-push "git push" "origin")
  (define-shell-command git-checkout "git checkout" "master"))

(defun git-grep (command)
  "Run git grep like grep"
  (interactive
   (list
    (read-from-minibuffer
     "Run git grep (like this): "
     "git grep -n -H -I -e ")))
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
     "Run git grep for dired (like this): "
     "git grep -l -e ")))
  (with-temp-buffer
    (shell-command (concat command " .") (current-buffer))
    (dired (cons "*git-grep-dired*"
                 (lines-to-list)))))

(defun git-add ()
  "Add the current file to the index"
  (interactive)
  (shell-command
   (concat "git add \""
           (file-name-nondirectory (or (buffer-file-name)
                                       default-directory))
           "\"")))

(defun git-diff (cachedp)
  "Run git diff on the current file or directory. With the prefix argument, run git diff --cached."
  (interactive "P")
  (let* ((cmd (if cachedp "git diff --cached ."
                "git diff ."))
         (bufname (concat "*" cmd "*"))
         (dir default-directory))
    (shell-command cmd bufname)
    (with-current-buffer (get-buffer bufname)
      (progn
        (cd dir)
        (diff-mode)))))

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
         ("\C-xgp" . git-pull)
         ("\C-xgP" . git-push)
         ("\C-xgS" . git-status)
         ("\C-xgs" . magit-status)
         )))

(define-minor-mode git-commands-mode
  "Some git commands bound to C-x g * for operating in the current directory."
  t
  nil
  git-commands-map)

(defun git-get-current-branch ()
  (with-temp-buffer
    (shell-command "git symbolic-ref HEAD" (current-buffer))
    (let ((str (chomp (buffer-string))))
      (string-match "^refs/.*?/\\(.*\\)$" str)
      (match-string 1 str))))

(defun git-set-rebase ()
  (interactive)
  (shell-command
   (concat "git config \"branch."
           (git-get-current-branch)
           ".rebase\" true")))

(provide 'git-commands)
