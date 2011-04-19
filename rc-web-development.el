(require 'visual-basic-mode)
(setq visual-basic-mode-indent 4)
(add-hook 'visual-basic-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(defun rc-sgml-mode-for-asp ()
  (interactive)
  (widen)
  (html-mode))

(add-to-auto-mode-alist
 '(("\\.asp$" . html-mode)
   ("\\.asa$" . visual-basic-mode)
   ("\\.vbs$" . visual-basic-mode)
   ("\\.vb$" . visual-basic-mode)
   ("\\.bas$" . visual-basic-mode)))

(add-hook 'sgml-mode-hook
	  (lambda ()
            (local-set-keys
             '(("C-c C-p" . sgml-skip-tag-backward)
               ("C-c C-n" . sgml-skip-tag-forward)))))

(defun enable-truncate-long-lines ()
  (toggle-truncate-lines 1))

(add-hook 'html-mode-hook 'enable-truncate-long-lines)

(put 'narrow-to-region 'disabled nil)

(require 'html-script)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(progn
  (require 'js2-mode)
  (set-variables
   '(js2-basic-offset 2)
   '(js2-bounce-indent-flag t)
   '(js2-mirror-mode nil))
  (add-hook 'js2-mode-hook 'turn-off-tabs)
  ;; (add-hook 'js2-mode-hook 'turn-on-c-subword-mode)
  ;; (add-hook 'js2-mode-hook 'turn-on-moz-minor-mode)
  (add-to-auto-mode-alist '(("\\.js\\'" . js2-mode))))

;; (progn
;;   (require 'espresso)
;;   (set-variables
;;    '(espresso-indent-level 2))
;;   (add-hook 'espresso-mode-hook 'turn-off-indent-tabs-mode)
;;   (add-hook 'espresso-mode-hook 'turn-on-c-subword-mode)
;;   (add-to-auto-mode-alist '(("\\.js\\'" . espresso-mode))))

;; (add-hook 'css-mode-hook 'turn-on-rainbow-mode)

(progn
  (require 'moz)
  (defun turn-on-moz () (interactive) (moz-minor-mode 1))

  (defun moz-send-string (string)
    "Send a string to Firefox via MozRepl."
    (comint-send-string (inferior-moz-process)
                        (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                                moz-repl-name ".setenv('printPrompt', false); "
                                moz-repl-name ".setenv('inputMode', 'multiline'); "
                                "undefined; \n"))
    ;; Give the previous line a chance to be evaluated on its own.  If
    ;; it gets concatenated to the following ones, we are doomed.
    (sleep-for 0 1)
    (comint-send-string (inferior-moz-process)
                        string)
    (comint-send-string (inferior-moz-process)
                        "\n--end-remote-input\n")
    (comint-send-string (inferior-moz-process)
                        (concat moz-repl-name ".popenv('inputMode', 'printPrompt'); "
                                "undefined; \n"))
    (comint-send-string (inferior-moz-process)
                        "\n--end-remote-input\n")
    (display-buffer (process-buffer (inferior-moz-process))))

  (defun moz-reload ()
    (interactive)
    (moz-send-string "content.location.reload()\n"))

  (defalias 'mozrepl-reload 'moz-reload)

  (defun moz-enter ()
    (interactive)
    (moz-send-string "repl.enter(content)\n"))

  (defalias 'mozrepl-enter 'moz-enter))

(provide 'rc-web-development)


