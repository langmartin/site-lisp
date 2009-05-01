(defun color-theme-langmartin ()
  "Color theme by Lang Martin, derived from color-theme-bharadwaj"
  (interactive)
  (color-theme-bharadwaj)
  (let ((color-theme-is-cumulative t)
        (match '((t (:background "lightgoldenrod2"))))
        (string '((t (:foreground "DarkRed"))))
        (string-b '((t (:bold t :foreground "DarkRed"))))
        (comment '((t (:foreground "grey50"))))
        )
   (color-theme-install
    `(color-theme-langmartin
      ((background-color . "ivory"))
      nil
      (erc-current-nick-face ((t (:bold t :foreground "Turquoise4"))))
      (erc-notice-face ,comment)
      (erc-timestamp-face ((t (:bold t :foreground "green4"))))
      (eshell-prompt ,string-b)
      (font-lock-comment-face ,comment)
      (font-lock-function-name-face ((t (:bold t :foreground "SlateBlue"))))
      (match ,match)
      (minibuffer-noticeable-prompt ,match)
      (org-agenda-restriction-lock ,match)
      (org-clock-overlay ,match)
      (org-todo ,string)
      ))))
