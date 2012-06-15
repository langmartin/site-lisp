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

(add-hooks
 '(lisp-mode-hook
   emacs-lisp-mode-hook
   scheme-mode-hook
   clojure-mode-hook
   slime-repl-mode-hook)
 'turn-on-paredit-mode)

(defun rc-slime-mode-unclobber-bindings ()
 (define-key slime-mode-map (kbd "H-.") 'find-tag)
 ;; (define-key slime-mode-map (kbd "H-.") 'slime-edit-definition)
 )

;; (add-hook 'slime-mode-hook 'rc-slime-mode-unclobber-bindings)

(global-set-key (kbd "H-.") 'find-tag)

(rc-bind-cleanup-untabify-save lisp-mode-map)
(rc-bind-cleanup-untabify-save emacs-lisp-mode-map)
(rc-bind-cleanup-untabify-save scheme-mode-map)
(rc-bind-cleanup-untabify-save clojure-mode-map)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(defun clojure-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (before (subseq segments 0 clojure-test-ns-segment-position))
         (after (subseq segments clojure-test-ns-segment-position))
         (test-segments (append (list (car before)) (list "test") (cdr before) after)))
    (mapconcat 'identity test-segments "/")))

(defun rc-clojure-indentation ()
  (interactive)
  (put-clojure-indent 'and-let 1)
  (put-clojure-indent 'valid-let 1))
(rc-clojure-indentation)

(provide 'rc-lisp)
