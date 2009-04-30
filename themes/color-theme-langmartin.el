(defun color-theme-langmartin ()
  "Color theme by Lang Martin, derived from color-theme-bharadwaj"
  (interactive)
  (color-theme-bharadwaj)
  (let ((color-theme-is-cumulative t)
        (match '(:background "lightgoldenrod2"))
        (string '(:foreground "DarkRed")))
   (color-theme-install
    `(color-theme-langmartin
      ((background-color . "ivory"))
      nil
      (erc-timestamp-face ((t (:bold t :foreground "green4"))))
      (erc-current-nick-face ((t (:bold t :foreground "Turquoise4"))))
      (eshell-prompt-face ((t (:bold t :foreground "DarkRed"))))
      (font-lock-comment-face ((t (:foreground "grey50"))))
      (font-lock-function-name-face ((t (:bold t :foreground "SlateBlue"))))
      (match ((t ,match)))
      (minibuffer-noticeable-prompt ((t ,match)))
      (org-agenda-restriction-lock ((t ,match)))
      (org-clock-overlay ((t ,match)))
      (org-todo ((t ,string)))
      ))))
