;; (add-to-list 'default-frame-alist '(font . "Monaco-15"))

(fringe-mode '(1 . 1))

(setenv "EMACS" "/usr/local/bin/emacs")
(setenv "EDITOR" "/usr/local/bin/emacsclient")
(setenv "GIT_EDITOR" (getenv "EDITOR"))
(setenv "PAGER" "tail -n100")
(setenv "GIT_PAGER" "")
(setenv "MANPAGER" "cat")
;; (setenv "SUDO_ASKPASS" "/Developer/usr/libexec/git-core/git-gui--askpass")

(defun rc-font-lg ()
  (interactive)
  (set-frame-font "Monaco-14"))

(defun rc-font-sm ()
  (interactive)
  (set-frame-font "Monaco-12"))

(custom-set-variables
 '(ns-alternate-modifier (quote hyper))
 '(ns-command-modifier (quote meta))
 '(tramp-initial-commands (quote ("unset HISTFILE" "unset correct" "unset autocorrect")))
 '(Info-additional-directory-list (quote ("/usr/share/info")))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man" "/usr/local/share/man")))
 '(woman-use-own-frame nil))

(global-set-key (kbd "H-SPC") 'just-one-space)

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
                         (throw 'handler (cdr pair))))
                   ns-input-file))))
      (if handler
          (funcall handler (pop ns-input-file))
        ad-do-it)))

  (setq ns-input-file-alist
        '(("^mailto:" . mailto-compose-mail))))

(provide 'rc-osx)
