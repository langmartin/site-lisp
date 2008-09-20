;; more shell enhancements
(require 'buffer-time-stamp)
(add-hook 'shell-mode-hook
          (lambda ()
            (buffer-time-stamp-mode t)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key "\C-a" 'eshell-bol)))

;; (defun kill-erc ()
;;   (kill-process-buffers
;;    (lambda () (equal major-mode 'erc-mode))
;;    (lambda ()
;;      (and (erc-server-process-alive)
;;           (erc-quit-server "emacs is dying")))))

;; (defun kill-scheme ()
;;   (kill-process-buffers
;;    (lambda () (equal major-mode 'inferior-scheme-mode))
;;    (lambda () (kill-buffer nil))))

;; (defun kill-process-buffers (match handle)
;;   (save-excursion
;;     (mapcar '(lambda (buffer)
;;                (set-buffer buffer)
;;                (and (funcall match) (funcall handle))
;;                t)
;;             (buffer-list))))

;; (defun kill-all-procs ()
;;   (kill-invisible-shell-buffers t)
;;   (kill-erc)
;;   (kill-scheme))

;; (require 'advice)
;; (defadvice save-buffers-kill-emacs (before kill-some-processes)
;;   (kill-all-procs))
;; (ad-activate 'save-buffers-kill-emacs)

;; (defun dave ()
;;   (interactive)
;;   (message "I can't let you do that, Dave"))
;; (global-set-key "\C-x\C-c" 'dave)


;;;; Shell customization
(defun kill-invisible-shell-buffers (&optional vis)
  (interactive)
  (save-excursion
    (mapcar '(lambda (buffer)
               (and (or vis (not (get-buffer-window buffer 'visible)))
                    (and (set-buffer buffer) (equal major-mode 'shell-mode))
                    (progn
                      (comint-bol)
                      (or (eobp) (kill-line))
                      (while (comint-check-proc buffer)
                        (comint-send-eof)
                        (sleep-for 0 2))
                      (kill-buffer buffer))))
            (buffer-list))))

(defun comint-send-something (char)
  (comint-send-input t t)
  (comint-send-string
   (get-buffer-process (current-buffer))
   char))

(defun comint-send-C-c ()
  (interactive)
  (comint-send-something ""))

(defun comint-send-C-z ()
  (interactive)
  (comint-send-something ""))

(add-hook
 'shell-mode-hook
 (lambda ()
   (local-set-key "\C-c\C-c" 'comint-send-C-c)
   (local-set-key "\C-c\C-z" 'comint-send-C-z)))

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(defun local-shell ()
  (interactive)
  (shell-and-thunk "local" (lambda () nil)))

(defun shell-and-thunk (name thunk)
  (let* ((final-name (concat "*shell " name "*"))
         (final-buffer (get-buffer final-name)))
    (if final-buffer
        (switch-to-buffer final-buffer)
      (progn
        (shell)
        (rename-buffer final-name)
        (funcall thunk)))))

(defun shell-and-ssh (name)
  (shell-and-thunk name
                   (lambda ()
                     (sleep-for 0 2)
                     (insert "cd; ssh " name))))

(defmacro make-shell-and-ssh-aliases (&rest names)
  `(progn
    ,@(mapcar (lambda (name)
                (let ((name (symbol-name name)))
                  `(defun ,(make-symbol (concat name "-shell")) ()
                     (interactive)
                     (shell-and-ssh ,name))))
              names)))

;; (make-shell-and-ssh-aliases
;;  liar
;;  volt
;;  flash
;;  wort
;;  quad
;;  cerf
;;  jerk
;;  tank
;;  nemo
;;  ogre
;;  foo
;;  fsck)

(defun liar-shell () (interactive) (shell-and-ssh "liar"))
(defun volt-shell () (interactive) (shell-and-ssh "volt"))
(defun flash-shell () (interactive) (shell-and-ssh "flash"))
(defun wort-shell () (interactive) (shell-and-ssh "wort"))
(defun hera-shell () (interactive) (shell-and-ssh "hera"))
(defun cerf-shell () (interactive) (shell-and-ssh "cerf"))
(defun iago-shell () (interactive) (shell-and-ssh "iago"))
(defun tank-shell () (interactive) (shell-and-ssh "tank"))
(defun nemo-shell () (interactive) (shell-and-ssh "nemo"))
(defun ogre-shell () (interactive) (shell-and-ssh "ogre"))
(defun fsck-shell () (interactive) (shell-and-ssh "fsck"))
(defun nike-shell () (interactive) (shell-and-ssh "nike"))
(defun omplus-shell () (interactive) (shell-and-ssh "omplus"))

;; (defun abla-shell () (interactive) (shell-and-ssh "abla"))
;; (defun rove-shell () (interactive) (shell-and-ssh "rove"))

(require 'term)

(term-set-escape-char ?\C-x)

(defun cx-term-host ()
  (interactive)
  (let ((str (buffer-name)))
   (string-match "\\*\\(.*\\)\\*" str)
   (let ((found (match-string 1 str)))
     (if (or (equal found "local") (equal found "terminal"))
         ""
       (concat "/" found ":")))))

(defun cx-term-cwd ()
  "set the working directory of term mode in trampily"
  (interactive)
  (goto-char (point-max))
  (term-send-raw-string "echo ::`pwd`::\n")
  (sleep-for 2)
  (goto-char (point-max))
  (re-search-backward "::\\(.*\\)::")
  (let* ((found (match-string 1))
         (found (concat (cx-term-host) found)))
    (message "current directory %s" found)
    (setq default-directory found))
  (goto-char (point-max)))

(defun cx-term-fix-cwd ()
  "fix cwd"
  (interactive)
  (setq default-directory "~"))

(provide 'shell-enhancements-for-lang)
