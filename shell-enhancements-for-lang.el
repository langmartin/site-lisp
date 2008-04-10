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

(provide 'shell-enhancements-for-lang)
