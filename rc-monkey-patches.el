(progn 
  (require 'magit)
  (defvar magit-editor "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
  (defun magit-interactive-rebase ()
    "Start a git rebase -i session, old school-style."
    (interactive)
    (unless (magit-server-running-p)
      (server-start))
    (let* ((section (get-text-property (point) 'magit-section))
           (commit (and (member 'commit (magit-section-context-type section))
                        (magit-section-info section)))
           (old-editor (getenv "GIT_EDITOR")))
      (setenv "GIT_EDITOR" (or magit-editor
                               (locate-file "emacsclient" exec-path)))
      (unwind-protect
          (magit-run-git-async
           "rebase" "-i"
           (or (and commit (concat commit "^"))
               (magit-read-rev "Interactively rebase to" (magit-guess-branch))))
        (if old-editor
            (setenv "GIT_EDITOR" old-editor)))))

  (defun magit-push ()
    (interactive)
    (message "This monkey-patched version of push only pushes what you have configured in .git/config for the default push. Do the rest on the command line.")
    (magit-run-git-async "push" "-v")))

(provide 'rc-monkey-patches)
