(progn
  (fringe-mode '(1 . 1))
  (update-alist 'default-frame-alist '(width . 162))
  (update-alist 'default-frame-alist '(height . 76)))

(set-variables
 '(ns-alternate-modifier (quote hyper))
 '(ns-command-modifier (quote meta))
 '(tramp-initial-commands (quote ("unset HISTFILE" "unset correct" "unset autocorrect")))
 '(sql-postgres-program "/command/psql")

 '(Info-additional-directory-list (quote ("/Applications/Emacs.app/Contents/Resources/info" "/usr/share/info")))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man" "/usr/local/share/man" "/opt/local/man" "/coptix/local/man")))
 '(woman-use-own-frame nil))
