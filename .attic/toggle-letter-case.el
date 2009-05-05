(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (put this-command
             'state
             (cond ((looking-at "[[:upper:]][[:upper:]]") "all caps")
                   ((looking-at "[[:upper:]][[:lower:]]") "init caps")
                   (t "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower")))))

(define-minor-mode toggle-letter-case-mode
  "use toggle-letter-case instead of the other case commands"
  :global t
  :init-value t
  :keymap `(("\M-c" . toggle-letter-case)
            ("\M-l" . toggle-letter-case)
            ("\M-u" . toggle-letter-case)))

(provide 'toggle-letter-case)
