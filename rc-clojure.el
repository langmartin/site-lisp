(require 'clojure-mode)

(add-hook 'clojure-mode-hook 'turn-on-paredit-mode-with-extra-braces)

(provide 'rc-clojure)
