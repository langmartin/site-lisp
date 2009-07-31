(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/Applications/Emacs.app/Contents/Resources/info" "/usr/share/info")))
 '(LaTeX-command "pdflatex")
 '(TeX-default-mode (quote plain-tex-mode))
 '(column-number-mode t)
 '(cx-timesheet-tunnel-extra "-L3128:iago:3128 -L3129:iago:3129 -L1025:mail:25 -L1110:iago:110")
 '(cx-timesheet-username "lang")
 '(dired-listing-switches "-alh")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#medium" "#scheme"))))
 '(erc-join-buffer (quote bury))
 '(erc-pals (quote ("jlongster" "bweaver")))
 '(erc-server-reconnect-timeout 300)
 '(erc-user-full-name "Lang Martin")
 '(eshell-ls-use-colors nil)
 '(eshell-prompt-function (lambda nil (concat (number-to-string eshell-last-command-status) " " (eshell/pwd) (if (= (user-uid) 0) " # " " $ "))))
 '(eshell-visual-commands (quote ("ssh" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm")))
 '(gnus-build-sparse-threads (quote some))
 '(gnus-dribble-directory "~/.emacs.d")
 '(gnus-fetch-old-headers (quote invisible))
 '(gnus-refer-thread-limit t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(icicle-reminder-prompt-flag 0)
 '(inhibit-startup-screen t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(line-spacing 1)
 '(mail-mailing-lists (quote ("gambit-list@iro.umontreal.ca" "all@coptix.com" "dns@list.cr.yp.to")))
 '(newsticker-url-list nil)
 '(ns-alternate-modifier (quote hyper))
 '(ns-command-modifier (quote meta))
 '(org-agenda-files (quote ("z:/IDMWS_Int/dds/docs/spec.txt")))
 '(paren-sexp-mode nil)
 '(partial-completion-mode t)
 '(pgg-default-user-id "Lang Martin")
 '(rst-mode-lazy nil)
 '(scroll-bar-mode nil)
 '(server-mode t)
 '(sql-postgres-program "/command/psql")
 '(starttls-gnutls-program "/coptix/local/bin/gnutls-cli")
 '(temporary-file-directory "~/.emacs.d/tmp/")
 '(tool-bar-mode nil)
 '(track-eol t)
 '(tramp-initial-commands (quote ("unset HISTFILE" "unset correct" "unset autocorrect")))
 '(w3m-use-cookies t)
 '(w3m-use-tab nil)
 '(w3m-use-tab-menubar nil)
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man" "/usr/local/share/man" "/opt/local/man" "/coptix/local/man")))
 '(woman-use-own-frame nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erc-notice-face ((t (:foreground "grey70")))))
