;; (add-to-list 'default-frame-alist '(font . "Monaco-15"))

(fringe-mode '(1 . 1))

(setenv "EMACS" "/Applications/Emacs.app/Contents/MacOS/Emacs")
(setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(setenv "GIT_EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(setenv "GIT_PAGER" "")
(setenv "PAGER" "cat")

(defun osx-defaults-write ()
  (interactive)
  (shell-command "defaults write com.apple.Dock pinning end")
  (shell-command "killall Dock")
  (shell-command "defaults write com.apple.iTunes hide-ping-dropdown 1")
  (shell-command "defaults write com.apple.iTunes show-store-link-arrows 1")
  (shell-command "sudo pmset hibernatemode 0")
  )

(custom-set-variables
 '(ns-alternate-modifier (quote hyper))
 '(ns-command-modifier (quote meta))
 '(tramp-initial-commands (quote ("unset HISTFILE" "unset correct" "unset autocorrect")))
 '(sql-postgres-program "/command/psql")
 '(Info-additional-directory-list (quote ("/Applications/Emacs.app/Contents/Resources/info" "/usr/share/info")))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man" "/usr/local/share/man" "/opt/local/man" "/coptix/local/man")))
 '(woman-use-own-frame nil))

(require 'mailto-compose-mail)
(provide 'rc-osx)
