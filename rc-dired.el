(require 'dired+)

(custom-set-variables
 '(dired-listing-switches "-alh"))

(add-hook 'dired-mode-hook 'turn-down-font-lock)

(provide 'rc-dired)
