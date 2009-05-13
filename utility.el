(defun with-output-file (path thunk)
  (save-excursion
    (set-buffer (create-file-buffer path))
    (funcall thunk)
    (setq buffer-file-name path)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun alphanumericp (ch)
  (or (and (>= ch ?a) (<= ch ?z))
      (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?0) (<= ch ?9))))

(defvar hex-values "0123456789ABCDEF")

(defun string-ref (string offset)
  (let ((idx offset))
    (catch
        'char
      (mapc (lambda (ch)
              (if (= idx 0)
                  (throw 'char ch)
                (setq idx (- idx 1))))
            string))))

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
                          path)))
        lst))

(defun set-variables (&rest lst)
  "Set a list of variables using the same syntax as custom-set-variables"
  (mapc (lambda (args)
          (set (car args)
               (eval (cadr args))))
        lst))

(provide 'utility)
