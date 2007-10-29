(defgroup cx-timesheet nil
  "Make timesheet entries at Coptix"
  :group 'tools
  :prefix "cx-timesheet-")

(defcustom cx-timesheet-username ""
  "Your timesheet username"
  :group 'cx-timesheet
  :type 'string)

(defun cx-timesheet-rc ()
  (setq auto-mode-alist
        (cons (cons "\\.action\\.el$" 'action-el-handler)
              auto-mode-alist)))

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

(defun tms-workstation (notes)
  (interactive "snote: ")
  (cx-timesheet-entry "592" notes))

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

(defun else () t)

(defun urlencode (string)
  (let ((out ""))
    (mapc (lambda (ch)
            (setq out
                  (concat
                   out
                   (cond ((alphanumericp ch)
                          (char-to-string ch))
                         ((= ?  ch) "+")
                         ((else) (concat
                                  "%"
                                  (hex-nibble (ash ch -4))
                                  (hex-nibble ch)))))))
          string)
    out))

(defun action-el-handler ()
  (save-excursion
    (emacs-lisp-mode)
    (eval-buffer)
    (kill-buffer (current-buffer))))

(provide 'cx-timesheet)
