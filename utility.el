(defun with-output-file (path thunk)
  (save-excursion
    (set-buffer (create-file-buffer path))
    (let ((standard-output (current-buffer)))
      (funcall thunk)
      (setq buffer-file-name path)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defmacro let-output-file (path &rest body)
  (declare (indent 1))
  `(with-output-file ,path (lambda () ,@body)))

(defun with-input-file (path thunk)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((standard-input (current-buffer)))
      (funcall thunk))))

(defmacro let-input-file (path &rest body)
  (declare (indent 1))
  `(with-input-file ,path (lambda () ,@body)))

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

(defun generalized-member (compare element sequence)
  (let ((lst sequence)
        (result nil))
    (while lst
      (if (funcall compare element (car lst))
          (progn
            (setq result lst)
            (setq lst nil))
        (setq lst (cdr lst))))
    result))

(assert
 (equal (generalized-member 'eql 'a '(b c a d))
        '(a d)))

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

(defun urldecode (string)
  (let ((state 0) (code nil))
    (flet ((hex (ch) (string-match (char-to-string ch) hex-values)))
      (mapconcat (lambda (ch)
                   (cond ((= state 0)
                          (cond ((= ?+ ch) " ")
                                ((= ?% ch) (setq state 1) "")
                                (t (char-to-string ch))))
                         ((= state 1)
                          (setq state 2 code ch) "")
                         ((= state 2)
                          (setq state 0)
                          (char-to-string
                           (+ (* 16 (hex code))
                              (* 1  (hex ch)))))))
                 string
                 ""))))

(assert (equal "foo+bar+%28baz%29" (urlencode "foo bar (baz)")))
(assert (equal "foo bar (baz)" (urldecode (urlencode "foo bar (baz)"))))

(defun global-set-keys (alist &rest hooks)
  "Set an alist of '(\"kbd\" . function) pairs globally. Locally
with the optional second argument.

You can find any kbd name by creating a keyboard macro, striking
the keys, and editing the macro with C-x C-k e. Examples include
the preceding, RET, <home>, and M-<f4>."
  (mapc (lambda (pair)
          (let ((binding (read-kbd-macro (car pair)))
                (cmd (cdr pair)))
            (funcall (if (boundp 'global-set-keys-cmd)
                         global-set-keys-cmd
                       'global-set-key)
                     binding
                     cmd)
            (if hooks
                (mapc (lambda (hook)
                        (add-hook hook
                                  `(lambda ()
                                     (local-set-key ,binding ',cmd))))
                      hooks))))
        alist))

(defun local-set-keys (alist)
  "See global-set-keys."
  (let ((global-set-keys-cmd 'local-set-key))
    (global-set-keys alist)))

(defun add-to-alist (list-sym element &optional append)
  (add-to-list list-sym element append
               (lambda (a b)
                 (eq (car a) (car b)))))

(defun update-alist (alist-symbol element &optional compare)
  (if (not compare) (setq compare 'eq))
  (set alist-symbol
       (cons element
             (filter (lambda (x)
                       (if (funcall compare (car x) (car element))
                           nil
                         x))
                     (symbol-value alist-symbol)))))

(defun add-to-auto-mode-alist (alist)
  "Update auto-mode-alist with pairs from the provided alist"
  (mapc (lambda (new)
          (update-alist 'auto-mode-alist
                        new
                        'equal))
        alist))

(defun add-to-path (lst &optional prepend)
  "Add a list of paths to the ends of PATH and exec-path"
  (let ((bound (cond ((equal system-configuration "i386-mingw-nt5.1.2600") ";")
                     (t ":"))))
    (mapc (lambda (path)
            (add-to-list 'exec-path path (if prepend nil 'append))
            (setenv "PATH"
                    (if prepend
                        (concat (expand-file-name path) bound (getenv "PATH"))
                      (concat (getenv "PATH") bound (expand-file-name path)))))
          lst)))

(defun set-variables (&rest lst)
  "Set a list of variables using the same syntax as custom-set-variables"
  (mapc (lambda (args)
          (set-default (car args) (eval (cadr args))))
        lst))

(defmacro save-default-directory (directory &rest body)
  "CD then restore the default-directory."
  (declare (indent 1))
  (let ((dir (gensym)))
    `(let ((,dir default-directory))
       (unwind-protect
           (progn (cd ,directory) ,@body)
         (cd ,dir)))))

(provide 'utility)
