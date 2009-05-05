(windmove-default-keybindings)
(setq pop-up-windows nil)
(setq bbdb-use-pop-up nil)

(defun clobber-all-gnus-popups ()
  "these are settings that are meant to suppress all Gnus popup
windows. See: http://www.emacswiki.org/cgi-bin/wiki/OneWindow"
  ;; Gnus from here on down
  (remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
  ;; do not destroy other windows
  (setq gnus-use-full-window nil)
  (defun my-selected-gnus-buffer-configuration (config)
    (let (result)
      (while (and (not result) config)
        (when (memq 'point config)
          (setq result config))
        ;; start recursively for any sublists
        (dolist (elem config)
          (when (and (not result) (listp elem))
            (setq result (my-selected-gnus-buffer-configuration elem))))
        (setq config (cdr config)))
      result))
  (setq gnus-buffer-configuration
        (mapcar (lambda (conf)
                  (let ((label (car conf))
                        (config (my-selected-gnus-buffer-configuration
                                 (cdr conf))))
                    (if config
                        (list label config)
                      conf)))
                gnus-buffer-configuration))
  ;; Now manually fix all those that have point in the wrong buffer,
  ;; usually the summary.
  (gnus-add-configuration '(article (article 1.0 point)))
  (gnus-add-configuration '(pipe (article 1.0 point)))
  (gnus-add-configuration '(score-trace (article 1.0 point)))
  (gnus-add-configuration '(score-words (article 1.0 point)))
  (gnus-add-configuration '(split-trace (article 1.0 point))))

(provide 'one-window)
