(require 'moz)

(add-hook 'js2-mode-hook
          (lambda ()
            (moz-minor-mode 1)))

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

(provide 'moz-rc)
