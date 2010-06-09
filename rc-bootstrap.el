(defun load-subdirs-in (dir)
  (let ((default-directory dir))
    (load (expand-file-name "subdirs.el") t t t)
    (load (expand-file-name "leim-list.el") t t t)))

(setq system-load-path (mapcar 'identity load-path))

(defun rc-load-subdirs ()
  (let ((tail load-path) dir)
    (while tail
      (setq dir (car tail))
      (if (not (member dir system-load-path))
          (load-subdirs-in dir))
      ;; We don't use a dolist loop and we put this "setq-cdr" command at
      ;; the end, because the subdirs.el files may add elements to the end
      ;; of load-path and we want to take it into account.
      (setq tail (cdr tail)))))

(defun add-to-load-path (path)
  "Add one path to load-path, expanding it and executing
subdirs.el and leim-list.el, if they exist."
  (setq path (expand-file-name path))
  (if (not (member path load-path))
      (progn
        (setq load-path (cons path load-path))
        (load-subdirs-in path))))

(defun add-to-info-path (path)
  "Add one path to Info-default-directory-list, and expand it."
  (setq path (expand-file-name path))
  (if (and (boundp 'Info-directory-list) Info-directory-list)
      (add-to-list 'Info-directory-list path)
    (add-to-list 'Info-default-directory-list path)))

(defun add-to-environment-path* (env boundary data &optional prepend)
  (if (not boundary)
      (setq boundary
            (cond ((equal system-configuration "i386-mingw-nt5.1.2600") ";")
                  (t ":"))))
  (mapc (lambda (path)
          (setq path (expand-file-name path))
          (setenv env
                  (if prepend (concat path boundary (getenv env))
                    (concat (getenv env) boundary path))))
        data))

(defun add-to-texinputs (&rest paths)
  "Add a list of paths to the TEXINPUTS environment variable."
  (add-to-environment-path* "TEXINPUTS" ";" paths))
