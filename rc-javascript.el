(defvar rc-javascript-mode)

(defun js-insert-lambda ()
  (interactive)
  (let ((before "function() {")
        (after  "}"))
    (insert before)
    (insert after)
    (backward-char (length after))))

(defun rc-js2-mode-setup ()
  (interactive)
  (require 'js2-mode)
  (custom-set-variables
   '(js2-bounce-indent-p t)
   '(js2-mirror-mode nil)
   '(js2-cleanup-whitespace t)
   '(js2-global-externs (quote (require exports))))
  (add-hook 'js2-mode-hook 'turn-on-c-subword-mode)
  (define-key js2-mode-map (kbd "H-l") 'js-insert-lambda)
  (eval-after-load 'js2-mode
    '(progn
       (require 'js2-imenu-extras)
       (js2-imenu-extras-setup))))

(defun rc-js2-mode-spaces ()
  "Configure js2 mode without tabs and with js2-basic-offset: 2"
  (interactive)
  (require 'js2-mode)
  (set-default 'js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'turn-off-tabs))

(defun rc-js2-mode-tabs ()
  "Configure js2 mode with tabs and with js2-basic-offset: 4"
  (interactive)
  (require 'js2-mode)
  (set-default 'js2-basic-offset 4)
  (add-hook 'js2-mode-hook 'turn-on-tabs)
  (add-hook 'js2-mode-hook 'set-tab-width-4))

(defun rc-js2-mode ()
  (rc-js2-mode-setup)
  (rc-js2-mode-tabs)
  (add-to-auto-mode-alist '(("\\.js\\'" . js2-mode)))
  (defalias 'rc-javascript-mode 'js2-mode))

(defun rc-espresso-mode ()
  (require 'espresso)
  (custom-set-variables
   '(espresso-indent-level 2))
  (add-hook 'espresso-mode-hook 'turn-off-indent-tabs-mode)
  (add-hook 'espresso-mode-hook 'turn-on-c-subword-mode)
  (add-to-auto-mode-alist '(("\\.js\\'" . espresso-mode)))
  (defalias 'rc-javascript-mode 'espresso-mode))

;; (add-hook 'css-mode-hook 'turn-on-rainbow-mode)

(defun rc-jshint-mode ()
  (interactive)
  (require 'js)
  (defun turn-on-flymake-mode () (interactive) (flymake-mode 1))
  (add-hook 'js-mode-hook 'turn-on-flymake-mode)
  (add-hook 'js-mode-hook 'set-tab-width-4)

  ;; (add-to-load-path "/usr/local/lib/node_modules/jshint-mode")
  ;; (require 'flymake-jshint)
  (add-to-auto-mode-alist '(("\\.js\\'" . js-mode)))

  (define-key js-mode-map (kbd "H-l") 'js-insert-lambda)
  (defalias 'rc-javascript-mode 'js-mode))

(rc-js2-mode)

(defun rc-mozilla-repl ()
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

(provide 'rc-javascript)
