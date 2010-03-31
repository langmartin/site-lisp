(add-to-list 'default-frame-alist '(font . "Consolas-11"))

(progn
  (setenv "EDITOR" "c:/mlm/Emacs/bin/emacsclientw.exe")
  (setenv "SSH_ASKPASS" "c:/mlm/Git/libexec/git-core/git-gui--askpass")
  (setenv "PAGER" "cat")
  (setenv "DISPLAY" "1"))

(setq tramp-default-method "sshx")
(setq erc-server-coding-system '(utf-8 . utf-8))
(setq sendmail-program "msmtp"
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

(mouse-avoidance-mode 1)
(setq visible-bell t)

(setq tramp-default-proxies-alist
      `((nil "\\`root\\'" "/sshx:%h:")))

(global-set-keys '(("C-x C-c" . nil)))

(setq w32-apps-modifier 'hyper)
