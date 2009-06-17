(defun with-output-file (path thunk)
  (save-excursion
    (set-buffer (create-file-buffer path))
    (prog1
        (funcall thunk)
      (setq buffer-file-name path)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun with-input-file (path thunk)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((standard-input (current-buffer)))
      (funcall thunk))))

(defun slurp-file-contents (path)
  (with-input-file
   path
   (lambda ()
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun slurp-file-list (path)
  (let ((result nil))
    (with-input-file
     path
     (lambda ()
       (goto-char (point-min))
       (while (not (eobp))
         (setq result
               (cons (filter-buffer-substring
                      (point)
                      (save-excursion
                        (end-of-visible-line)
                        (point)))
                     result))
         (forward-line))))
    result))

(defun read-all (&optional stream)
  (let ((result nil))
    (condition-case nil
        (while t
          (setq result
                (cons (read stream)
                      result)))
      (error result))))

(defun alphanumericp (ch)
  (or (and (>= ch ?a) (<= ch ?z))
      (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?0) (<= ch ?9))))

(defun whitespacep (ch)
  (member ch '(?  ?\t ?\n ?\r ?\f ?\v)))

(defun trim (str)
  (string-match "^[[:space:]]*\\(.*?\\)[[:space:]]*$" str)
  (match-string 1 str))

(defun chomp (str)
  (string-match "^\\(.*?\\)[[:space:]\r\n]*$" str)
  (match-string 1 str))

(defvar hex-values "0123456789ABCDEF")

(defalias 'string-ref 'aref)

(defun hex-nibble (ch)
  (char-to-string
   (string-ref
    hex-values
    (logand 15 ch))))

(defun urlencode (string)
  (mapconcat (lambda (ch)
               (cond ((alphanumericp ch)
                      (char-to-string ch))
                     ((= ?  ch) "+")
                     (t (concat
                         "%"
                         (hex-nibble (ash ch -4))
                         (hex-nibble ch)))))
             string
             ""))

(assert (equal "foo+bar+%28baz%29" (urlencode "foo bar (baz)")))

(defun global-set-keys (alist &optional local)
  "Set an alist of '(\"kbd\" . function) pairs globally. Locally
with the optional second argument.

You can find any kbd name by creating a keyboard macro, striking
the keys, and editing the macro with C-x C-k e. Examples include
the preceding, RET, <home>, and M-<f4>."
  (mapc (lambda (pair)
          (funcall
           (if local 'local-set-key 'global-set-key)
           (read-kbd-macro (car pair))  ; the function called by kbd
           (cdr pair)))
        alist))

(defun add-to-auto-mode-alist (lst)
  "Add an alist to the front of auto-mode-alist"
  (mapc (lambda (new)
          (setq auto-mode-alist
                (cons new
                      auto-mode-alist)))
        lst))

(defun add-to-path (lst)
  "Add a list of paths to the ends of PATH and exec-path"
  (mapc (lambda (path)
          (add-to-list 'exec-path path 'append)
          (setenv "PATH"
                  (concat (getenv "PATH")
                          (cond ((equal system-configuration "i386-mingw-nt5.1.2600") ";")
                                (t ":"))
                          (expand-file-name path))))
        lst))

(defun set-variables (&rest lst)
  "Set a list of variables using the same syntax as custom-set-variables"
  (mapc (lambda (args)
          (set (car args)
               (eval (cadr args))))
        lst))

(provide 'utility)
