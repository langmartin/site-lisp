(defun color-theme-langmartin ()
  "Color theme by Lang Martin, derived from color-theme-bharadwaj"
  (interactive)
  (color-theme-bharadwaj)
  (let ((color-theme-is-cumulative t))
   (color-theme-install
    '(color-theme-langmartin
      ((background-color . "ivory"))
      nil
      (erc-timestamp-face ((t (:bold t :foreground "green4"))))
      (erc-current-nick-face ((t (:bold t :foreground "Turquoise3"))))
      (font-lock-comment-face ((t (:foreground "grey45"))))
      (font-lock-function-name-face ((t (:bold t :foreground "SlateBlue"))))
      (match ((t (:background "lightgoldenrod2"))))
      (minibuffer-noticeable-prompt ((t (:background "lightgoldenrod2"))))
      (org-agenda-restriction-lock ((t (:background "lightgoldenrod2"))))
      (org-clock-overlay ((t (:background "lightgoldenrod2"))))
      (org-todo ((t (:bold t :foreground "Red3"))))
      ))))
