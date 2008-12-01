(defun rc-term-clobber-keymaps ()
  ;; (setq term-mode-map nil
;;         term-raw-map nil
;;         term-raw-escape-map nil
;;         term-pager-break-map nil)
  (unload-feature 'term t)
  (require 'term))

(defun rc-term-more-keys ()
  (define-key term-raw-escape-map "\C-y" 'term-paste)
  (define-key term-raw-escape-map "\M-f" 'cx-term-cwd))

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

(defun cx-term-host ()
  (interactive)
  (let ((str (buffer-name)))
   (string-match "[*]\\(.*?\\)[* ]" str)
   (let ((found (match-string 1 str)))
     (if (or (equal found "local") (equal found "terminal"))
         ""
       (concat "/" found ":")))))

(defun cx-term-cwd ()
  "set the working directory of term mode in trampily"
  (interactive)
  (term-send-raw-string "echo ::`pwd`::\n")
  (sleep-for 0.3)
  (goto-char (point-max))
  (re-search-backward "::\\(.*\\)::")
  (let* ((found (match-string 1))
         (found (concat (cx-term-host) found)))
    (message "current directory %s" found)
    (setq default-directory found)))

;;;; default-directory mangling & pasting
(defun default-directory (&optional home)
  "Show and (with the prefix) reset the default directory of the current buffer"
  (interactive "P")
  (if home (setq default-directory "~/"))
  (message "%s" default-directory))

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
