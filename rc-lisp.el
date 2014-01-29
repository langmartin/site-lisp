(require 'paredit)
(require 'clojure-mode)
(require 'scheme-rc)
(rc-guile)

(defun rc-bind-paredit-extra-braces ()
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square-and-newline)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline))

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
  "Returns the path of the test file for the given namespace."
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (segments (cons (car segments) (cons "test" (cdr segments)))))
    (format "%stest/%s.clj"
            (file-name-as-directory
             (locate-dominating-file buffer-file-name "src/"))
            (mapconcat 'identity segments "/"))))

(defun rc-clojure-indentation ()
  (interactive)
  (put-clojure-indent 'and-let 1)
  (put-clojure-indent 'valid-let 1))

(rc-clojure-indentation)

(defun clojure-insert-lambda ()
  (interactive)
  (let ((before "(fn []")
        (after  ")"))
    (insert before)
    (insert after)
    (backward-char (length after))))

(define-key clojure-mode-map (kbd "H-l") 'clojure-insert-lambda)

(defun insert-clojure-clear-ns ()
  (interactive)
  (insert "(doseq [[x _] (ns-map *ns*)] (ns-unmap *ns* x))"))

(defun rc-clojure-cider ()
  (unless (package-installed-p 'cider)
    (package-install 'cider))
  (require 'cider)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-popup-stacktraces t
        cider-repl-popup-stacktraces t
        cider-repl-result-prefix ";; => "
        cider-auto-select-error-buffer t
        cider-repl-display-in-current-window t)

  (setenv "JVM_OPTS" "-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n"))

(rc-clojure-cider)

(provide 'rc-lisp)
