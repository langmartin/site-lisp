(defun rc-backups-and-autosave-directory (backup)
  "Set all the variables to move backups & autosave files out of
the working directory"
  (let ((backup (if (eql "/" (aref backup (- (length backup) 1)))
                    backup
                  (concat backup "/"))))
   (make-directory backup t)
   (setq backup-by-copying t
         delete-old-versions t
         kept-new-versions 10
         kept-old-versions 2
         version-control t
         backup-directory-alist `(("." . ,backup))
         tramp-backup-directory-alist backup-directory-alist
         auto-save-list-file-prefix (concat backup ".auto-saves-")
         auto-save-file-name-transforms `(("(.*)" ,(concat backup "\\1") t))
         vc-make-backup-files t)))

(defun backup-buffer-force ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'backup-buffer-force)
(rc-backups-and-autosave-directory "~/.emacs.d/backup")

(provide 'rc-backup)
