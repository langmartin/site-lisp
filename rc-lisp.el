(require 'paredit)
(require 'clojure-mode)
(require 'scheme-rc)
(rc-scheme48)

(defun rc-bind-paredit-extra-braces ()
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square-and-newline)
  (define-key paredit-mode-map (kbd "C-M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "C-M-}") 'paredit-close-curly-and-newline))

(rc-bind-paredit-extra-braces)

(defun turn-on-paredit-mode () (interactive) (paredit-mode 1))

(setq lisp-mode-hooks
      '(lisp-mode-hook
        emacs-lisp-mode-hook
        scheme-mode-hook
        clojure-mode-hook))

(add-hooks lisp-mode-hooks 'turn-on-paredit-mode)
(add-hook 'slime-repl-mode-hook 'turn-on-paredit-mode)

(rc-bind-cleanup-untabify-save lisp-mode-map)
(rc-bind-cleanup-untabify-save emacs-lisp-mode-map)
(rc-bind-cleanup-untabify-save scheme-mode-map)
(rc-bind-cleanup-untabify-save clojure-mode-map)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(provide 'rc-lisp)
