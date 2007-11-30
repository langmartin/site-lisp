(defgroup cx-timesheet nil
  "Make timesheet entries at Coptix"
  :group 'tools
  :prefix "cx-timesheet-")

(defcustom cx-timesheet-username ""
  "Your timesheet username"
  :group 'cx-timesheet
  :type 'string)

(defcustom cx-timesheet-host "ccc.coptix.lan"
  "Hostname to use normally"
  :group 'cx-timesheet
  :type 'string)

(defcustom cx-timesheet-output-path "~/.emacs.d/tms-output"
  "Path to put timesheet project dump"
  :group 'cx-timesheet
  :type 'string)

(defcustom cx-timesheet-tunnel-port 5551
  "Localhost port to use for tunneling"
  :group 'cx-timesheet
  :type 'number)

(defcustom cx-timesheet-tunnel-host "iago.hosts.coptix.com"
  "Hostname to use for tunneling"
  :group 'cx-timesheet
  :type 'string)

(defcustom cx-timesheet-tunnel-extra "-L3128:iago:3128 -D1080"
  "Extra arguments to the tunnel process"
  :group 'cx-timesheet
  :type 'string)

(defvar cx-timesheet-host-buffer cx-timesheet-host)

(defun cx-timesheet-rc ()
  "Prepend action-el-handler to auto-mode-alist"
  (setq auto-mode-alist
        (cons (cons "\\.action\\.el$" 'action-el-handler)
              auto-mode-alist)))

(defun cx-timesheet-start-tunnel ()
  "Start the tunnel"
  (interactive)
  (shell-command "killall emacs-ssh-tunnel")
  (let* ((port (number-to-string cx-timesheet-tunnel-port))
         (cmd (concat "argv0 ssh emacs-ssh-tunnel -fN -L " port ":ccc:80 "
                      cx-timesheet-tunnel-extra " "
                      cx-timesheet-tunnel-host " &")))
    (message cmd)
    (shell-command cmd)
    (setq cx-timesheet-host-buffer (concat "localhost:" port))))

(defun cx-timesheet-stop-tunnel ()
  "Stop using the tunnel"
  (interactive)
  (setq cx-timesheet-host-buffer cx-timesheet-host)
  (shell-command "killall emacs-ssh-tunnel"))

(defun action-el-handler ()
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
  (save-excursion
    (eval-buffer)
    (kill-buffer nil)))

(defvar cx-timesheet-debug)
(setq cx-timesheet-debug nil)

(defun cx-timesheet-entry (tskid notes)
  (let ((req (concat "http://" cx-timesheet-host-buffer "/timesheet/qs_tms_post.asp?"
                     "username=" cx-timesheet-username "&"
                     "tskid=" tskid "&"
                     "notes=" (urlencode notes))))
    (message "%s" req)
    (let* ((backto (current-buffer))
           (buffer (url-retrieve-synchronously req)))
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
         (url-retrieve-synchronously
          (concat "http://" cx-timesheet-host-buffer "/timesheet/qs_tms_items.asp"))))
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
          (with-output-file
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

(defun with-output-file (path thunk)
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

;; (urlencode "foo bar (baz)")

(provide 'cx-timesheet)
