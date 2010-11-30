(require 'slime)
(slime-setup '(slime-repl slime-js))

;;;; See https://github.com/ivan4th/swank-js. If I get node.js
;;;; running, I can use it to reflect the js repl into browsers
;;;; visiting the node. It should run cross-browser, so this is a way
;;;; to hook up mobile debugging.

(global-set-key (kbd "<f9>") 'slime-js-reload)
(defun turn-on-slime-js () (slime-js-minor-mode 1))
(add-hook 'js2-mode-hook 'turn-on-slime-js)

(provide 'rc-slime)
