(defgroup cx-timesheet nil
  "Make timesheet entries at Coptix"
  :group 'tools
  :prefix "cx-timesheet-")

(defcustom cx-timesheet-username ""
  "Your timesheet username"
  :group 'cx-timesheet
  :type 'string)

(defcustom cx-timesheet-output-path "~/.emacs.d/tms-output"
  "Path to put timesheet project dump"
  :group 'cx-timesheet
  :type 'string)

(defun cx-timesheet-rc ()
  (setq auto-mode-alist
        (cons (cons "\\.action\\.el$" 'action-el-handler)
              auto-mode-alist)))

(defun action-el-handler ()
  (emacs-lisp-mode)
  (eval-buffer)
  (kill-buffer (current-buffer)))

(defvar cx-timesheet-debug)
(setq cx-timesheet-debug nil)

(defun cx-timesheet-entry (tskid notes)
  (let ((req (concat "http://ccc.coptix.lan/timesheet/qs_tms_post.asp?"
                     "username=" cx-timesheet-username "&"
                     "tskid=" tskid "&"
                     "notes=" (urlencode notes))))
    (message "%s" req)
    (let ((buffer (url-retrieve-synchronously req)))
      (if cx-timesheet-debug
          (progn
            (view-buffer buffer))
        (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (if (not (search-forward-regexp "^Successfully" (point-max) t))
              (error "Timesheet entry failed"))
          (kill-buffer (current-buffer)))))))

(defun cx-timesheet-update ()
  "Update list of available projects"
  (interactive)
  (make-directory cx-timesheet-output-path t)
  (let ((req-buffer
         (url-retrieve-synchronously "http://ccc/timesheet/qs_tms_items.asp")))
    (save-excursion
      (set-buffer req-buffer)
      (goto-char (point-min))
      (search-forward "

" (point-max) t)
      (while (not (= (point) (point-max)))
        (cx-timesheet-convert-line)))))

(defun cx-tms-match ()
  (if (search-forward-regexp "^cxtms_\\.?\\(.*?\\)|\\([0-9]+\\).*?$" (point-at-eol) t)
      (let ((file (match-string 1))
            (id (match-string 2)))
        (cons (replace-regexp-in-string "/" "-" file)
              (number-to-string (string-to-number id))))
    nil))

(defun cx-timesheet-convert-line ()
  (let ((strings (cx-tms-match)))
    (if strings
        (let ((path (concat cx-timesheet-output-path "/"
                            (car strings)
                            ".action.el")))
          (call-with-output-file
           path
           (lambda ()
             (prin1
              `(call-interactively
                '(lambda (notes)
                   (interactive "snotes: ")
                   (cx-timesheet-entry ,(cdr strings) notes)))
              'insert))))
      (progn
        (beginning-of-line)
        (next-line)))))

;; cxtms_.CX Business:General > Administration: Accounting/Taxes|576
;; (progn (beginning-of-line) (previous-line) (cx-timesheet-convert-line))

(defun call-with-output-file (path thunk &optional extension)
  (save-excursion
    (set-buffer (create-file-buffer path))
    (funcall thunk)
    (setq buffer-file-name path)
    (save-buffer)
    (kill-buffer (current-buffer))))

;;;; urlencoding
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
  (let ((out ""))
    (mapc (lambda (ch)
            (setq out
                  (concat
                   out
                   (cond ((alphanumericp ch)
                          (char-to-string ch))
                         ((= ?  ch) "+")
                         (t (concat
                                  "%"
                                  (hex-nibble (ash ch -4))
                                  (hex-nibble ch)))))))
          string)
    out))

;; (urlencode "foo bar (baz)")

(provide 'cx-timesheet)
