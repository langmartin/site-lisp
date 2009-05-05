(defun rc-screen-like-C-t (set-key)
  "Set a C-t map of buffer commands to make things work a bit
like GNU screen with a C-t command key."
  (funcall set-key "\C-t" nil)
  (funcall set-key "\C-tt" 'transpose-chars)
  (funcall set-key "\C-t\C-t" 'iswitchb-buffer)
  (funcall set-key "\C-t\C-i" 'other-window)
  (funcall set-key "\C-tn" 'next-buffer)
  (funcall set-key "\C-t\C-n" 'next-buffer)
  (funcall set-key "\C-tp" 'previous-buffer)
  (funcall set-key "\C-t\C-p" 'previous-buffer)
  (funcall set-key "\C-tS" 'split-window-vertically)
  (funcall set-key "\C-tX" 'delete-window)
  ;; these are kind of my own riff on screen commands
  (funcall set-key "\C-tr" 'iswitchb-display-buffer)
  ;; these are like vi's f
  ;; (funcall set-key "\C-tf" 'mark-and-search-forward)
  ;; (funcall set-key "\C-tb" 'mark-and-search-backward)
  )

(rc-screen-like-C-t 'global-set-key)

(provide 'rc-screen-like-C-t)
