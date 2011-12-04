;; (add-to-list 'default-frame-alist '(font . "Monaco-15"))

(fringe-mode '(1 . 1))

(setenv "EMACS" "/Applications/Emacs.app/Contents/MacOS/Emacs")
(setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(setenv "GIT_EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(setenv "GIT_PAGER" "")
(setenv "PAGER" "cat")
;; (setenv "SUDO_ASKPASS" "/Developer/usr/libexec/git-core/git-gui--askpass")

(defun osx-defaults-write ()
  (interactive)
  (shell-command "defaults write com.apple.Dock pinning end")
  (shell-command "killall Dock")
  (shell-command "defaults write com.apple.iTunes hide-ping-dropdown 1")
  (shell-command "defaults write com.apple.iTunes show-store-link-arrows 1")
  (shell-command "sudo pmset hibernatemode 0")
  ;; (shell-command "defaults write com.apple.loginwindow LoginwindowLaunchesRelaunchApps -bool false")
  )

(custom-set-variables
 '(ns-alternate-modifier (quote hyper))
 '(ns-command-modifier (quote meta))
 '(tramp-initial-commands (quote ("unset HISTFILE" "unset correct" "unset autocorrect")))
 '(sql-postgres-program "/command/psql")
 '(Info-additional-directory-list (quote ("/Applications/Emacs.app/Contents/Resources/info" "/usr/share/info")))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man" "/usr/local/share/man")))
 '(woman-use-own-frame nil))

(progn
  (require 'advice)
  (require 'mailto-compose-mail)
  
  (defvar ns-input-file-alist nil)

  (defadvice ns-find-file (around ns-find-file-url activate)
    (message "ns-find-file-url")
    (message ns-input-file)
    (let ((handler
           (catch 'handler
             (mapc (lambda (pair)
                     (if (string-match (car pair) ns-input-file)
                         (throw 'handler (cdr pair))))))))
      (if handler
          (funcall handler (pop ns-input-file))
        ad-do-it)))

  (setq ns-input-file-alist
        '(("^mailto:" . mailto-compose-mail))))

(provide 'rc-osx)
