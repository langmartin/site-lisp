(require 'js2-mode)

;;; if you just set these, they're buffer local. If you stick them in
;;; the hook, they're not overridable (easily). So, we do a second
;;; customize block.

(add-to-auto-mode-alist
 '(("\\.js\\'" . js2-mode)))

(set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-mirror-mode nil))

(defun turn-on-c-subword-mode ()
  (interactive)
  (c-subword-mode 1))

(add-hook 'js2-mode-hook 'turn-off-indent-tabs-mode)
(add-hook 'js2-mode-hook 'turn-on-c-subword-mode)

(require 'moz)

(defun turn-on-moz-minor-mode ()
  (moz-minor-mode 1))

(add-hook 'js2-mode-hook 'turn-on-moz-minor-mode)

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

(defun moz-reload-page ()
  (interactive)
  (moz-send-string "content.location.href = content.location.href\n"))

(provide 'rc-javascript)
