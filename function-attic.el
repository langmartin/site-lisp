(defun rc-vi-mode-by-default ()
  "Add hooks to turn on vi-mode when entering major modes."
  (interactive)
  (mapcar '(lambda (x)
	     (add-hook x 'vi-mode t))
	  '(php-mode-hook
	    perl-mode-hook
            c-mode-hook
	    lisp-mode-hook
	    emacs-lisp-mode-hook
	    scheme-mode-hook
	    sh-mode-hook
	    text-mode-hook
	    html-mode-hook
            plain-tex-mode-hook
            latex-mode-hook)))

(defun rc-vi-mode ()
  "Setup Lang's preferences for less invasive vi-mode."
  (interactive)
  (global-set-key [f3] 'vi-mode)
  (global-set-key "OR" 'vi-mode)
  (setq vi-mode-old-tab-keybinding nil)
  (defadvice vi-mode (before vi-mode-save-old-indent)
    (setq vi-mode-old-tab-keybinding (key-binding "	")))
  (defadvice vi-mode (after vi-mode-rc-keybindings)
    (local-set-key ":"   'vi-unimplemented)
    (local-set-key "	" vi-mode-old-tab-keybinding))
  (ad-activate 'vi-mode)
  (vi-mode)
  (defun vi-forward-word (count)
    "Lang's replacement vi-forward-word that uses forward-word."
    (interactive "p")
    (if (forward-word count)
	(if (= 119 (char-syntax (char-after (+ 1 (point)))))
	    (forward-char)
	  t)
      nil))
  (vi-back-to-old-mode))
