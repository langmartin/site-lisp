(add-to-list 'default-frame-alist '(font . "Consolas-11"))

(progn
  (setenv "EDITOR" "c:/foo/emacs/bin/emacsclientw.exe")
  (setenv "SSH_ASKPASS" "c:/foo/Git/libexec/git-core/git-gui--askpass")
  (setenv "PAGER" "cat")
  (setenv "DISPLAY" "1"))

(setq tramp-default-method "sshx")
(setq erc-server-coding-system '(utf-8 . utf-8))
(setq sendmail-program "msmtp"
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

(mouse-avoidance-mode 'jump)
(setq visible-bell t)

;; try to improve slow performance on windows.
;; (setq w32-get-true-file-attributes nil)

(setq tramp-default-proxies-alist
      `((nil "\\`root\\'" "/sshx:%h:")))

(setq w32-apps-modifier 'hyper)

(defun w32-explorer ()
  (interactive)
  (w32-shell-execute
   "open" "explorer"
   (concat "/select,"
           (convert-standard-filename
            (or buffer-file-name default-directory)))))

(global-set-keys '(("H-e" . w32-explorer)))

(defun etc-hosts ()
  (interactive)
  (find-file
   (concat (getenv "windir")
           "/system32/drivers/etc/hosts")))

;; Use the default browser for local links and the explicit top-level
;; domain here, (it's IE6), chrome for everything else.
(progn
  (setq browse-url-generic-program "e:/portable/Chrome.exe")
  (setq browse-url-browser-function
        '(("^[^.]+$" . browse-url-default-windows-browser)
          ("\\.tva\\.gov" . browse-url-default-windows-browser)
          ("." . browse-url-generic))))

(defun copy-cmd-env ()
  (interactive)
  (kill-new
   (mapconcat (lambda (x)
                (concat "set " x "=" (getenv x) "\n"))
              `("HOME"
                "GNUPGHOME"
                "PATH"
                "TEMP"
                "TMP"
                "SSH_AUTH_SOCK"
                "CPATH"
                "INCLUDE"
                "LIB"
                "LIBRARY_PATH")
              "")))

(add-to-auto-mode-alist '(("\\.ini$" . conf-windows-mode)))

(provide 'rc-win)
