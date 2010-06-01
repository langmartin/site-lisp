(setq system-load-path (mapcar 'identity load-path))

(defun rc-load-subdirs ()
  (let ((tail load-path) dir)
    (while tail
      (setq dir (car tail))
      (if (not (member dir system-load-path))
          (let ((default-directory dir))
            (load (expand-file-name "subdirs.el") t t t)
            (load (expand-file-name "leim-list.el") t t t)))
      ;; We don't use a dolist loop and we put this "setq-cdr" command at
      ;; the end, because the subdirs.el files may add elements to the end
      ;; of load-path and we want to take it into account.
      (setq tail (cdr tail)))))
