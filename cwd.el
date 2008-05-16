;;;; cwd utility to get the tramp path back & forth twixt shells

;;;; You need to put emacs-paste-buffer.plist into
;;;; ~/Library/LaunchAgents, and you need to run SOMETHING to load it.
;;;; You should fix the path to the tmp file before you do so. Once
;;;; that puppy is loaded, you need to wrap ssh so that all
;;;; connections have an appended -R port:localhost:port befitting
;;;; your username.

;;;; Please ask lang for help setting this up if you want it.

(defun cwd ()
  "Copy the local version of the current working directory to the paste buffer"
  (interactive)
  (kill-new (localized-pwd)))

(defun localized-pwd ()
  (directory-file-name
   (or (tramp-localname default-directory)
       default-directory)))

(defun tramp-maybe-part (proc path-str)
  (if (eq 0 (string-match tramp-file-name-regexp path-str))
      (let ((path (tramp-dissect-file-name path-str)))
        (funcall proc path))
    nil))

(defun tramp-localname (path)
  (tramp-maybe-part 'tramp-file-name-localname path))

(defun tramp-hostname (path)
  (tramp-maybe-part 'tramp-file-name-host path))

(defun file-contents-to-string (file)
  (save-excursion
    (find-file file)
    (prog1
        (buffer-string)
      (kill-buffer nil))))

(defun find-pasted-file ()
  (interactive)
  (find-file (file-contents-to-string "~/tmp/emacs-paste-buffer.txt")))

(defun cwdd ()
  "Like cwd, but prepend the hostname"
  (interactive)
  (kill-new
   (let ((host (tramp-hostname default-directory)))
     (if host
         (concat "/" host ":" (tramp-localname default-directory))
       default-directory))))

(provide 'cwd)
