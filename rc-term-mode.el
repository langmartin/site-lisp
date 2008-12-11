(defun rc-term-clobber-keymaps ()
  ;; (setq term-mode-map nil
;;         term-raw-map nil
;;         term-raw-escape-map nil
;;         term-pager-break-map nil)
  (unload-feature 'term t)
  (require 'term))

(defun rc-term-more-keys ()
  (define-key term-raw-escape-map "\C-y" 'term-paste)
  (define-key term-raw-escape-map "\M-f" 'rc-term-cwd))

(defun rc-term-more-emacsy-keys ()
  (interactive)
  (term-set-escape-char ?\C-x)
  (rc-term-more-keys)
  (define-key term-raw-map "\M-x" 'execute-extended-command)
  (define-key term-raw-map "\M-`" 'other-frame))

(defun rc-term-more-termy-keys ()
  (interactive)
  (rc-term-clobber-keymaps)
  (term-set-escape-char ?\C-c)
  (rc-term-more-keys))

(require 'term)
(rc-term-more-emacsy-keys)

(defvar rc-term-hostname-wrap
  (lambda (host)
    (concat "/" host ":"))
"Procedure to wrap the hostname, when identified. A place to
switch out smb or sshfs for tramp.")
(make-variable-buffer-local 'rc-term-hostname-wrap)

(defun rc-term-use-tramp ()
  "Set rc-term-hostname-wrap to the default procedure"
  (interactive)
  (setq rc-term-hostname-wrap
        (lambda (host)
          (concat "/" host ":"))))

(defun rc-term-use-smb (mount)
  "Set rc-term-hostname-wrap to use paths in /Volumes"
  (interactive)
  (setq rc-term-hostname-wrap
        (lambda (host)
          (concat "/Volumes" host "/"))))

(defvar rc-term-hostname "hostname")
(make-variable-buffer-local 'rc-term-hostname)

(defun rc-term-fetch-data (command)
  (term-send-raw-string (concat "echo ::`" command "`::\n"))
  (sleep-for 0.3)
  (goto-char (point-max))
  (re-search-backward "::\\(.*\\)::")
  (match-string 1))

(defun rc-term-rename-buffer ()
  "automatically rename the buffer to *`hostname`*"
  (interactive)
  (rename-buffer (concat "*" (rc-term-fetch-data "hostname") "*")))

(defun rc-term-cwd ()
  "set the working directory of term mode in trampily"
  (interactive)
  (let ((str (buffer-name)))
    (string-match "[*]\\(.*?\\)[* ]")
    (let ((found (match-string 1 str)))
      (if (equal found "terminal")
          (rc-term-rename-buffer)
        (let ((dir (if (equal found "local")
                       ""
                     (funcall rc-term-hostname-wrap found))))
          (message "current directory %s" found)
          (setq default-directory found))))))

;;;; default-directory mangling & pasting
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

;; (defun file-contents-to-string (file)
;;   (save-excursion
;;     (find-file file)
;;     (prog1
;;         (buffer-string)
;;       (kill-buffer nil))))

;; (defun find-pasted-file ()
;;   (interactive)
;;   (find-file (file-contents-to-string "~/tmp/emacs-paste-buffer.txt")))

;; (defun cwdd ()
;;   "Like cwd, but prepend the hostname"
;;   (interactive)
;;   (kill-new
;;    (let ((host (tramp-hostname default-directory)))
;;      (if host
;;          (concat "/" host ":" (tramp-localname default-directory))
;;        default-directory))))

(provide 'rc-term-mode)
