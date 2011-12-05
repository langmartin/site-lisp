(require 'clojure-mode)

(defun turn-on-clojure-paredit-mode ()
  (interactive)
  (paredit-mode 1)
  (local-set-keys
   '(("M-{" . paredit-wrap-curly)
     ("M-}" . paredit-close-curly-and-newline)
     ("M-[" . paredit-wrap-square)
     ("M-]" . paredit-close-square-and-newline))))

(add-hook 'clojure-mode-hook 'turn-on-clojure-paredit-mode)

(provide 'rc-clojure)
