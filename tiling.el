;; Bugs:

;;   Switching windows immediately after blessing the second window will
;;   use other-window until it returns to the active window as stored in
;;   the window configuration. It's necessary to recapture on bless as
;;   well.

;;   It would be reasonable if intentionally switching a buffer updated
;;   the tiling. Popups should stay ignored.

;;;; Utilities

(defun rotate-list (lst)
  (append (cdr lst)
          (list (car lst))))

(defun pop-list-named (list-name)
  (set list-name
       (cdr (symbol-value list-name))))

(defun pairp (obj)
  (and (listp obj) (not (null obj))))

(defun current-window ()
  (car (window-list)))

(defun empty-prefixp (p) (= p 1))
(defun single-prefixp (p) (> p 1))
(defun double-prefixp (p) (>= p 16))
(defun triple-prefixp (p) (>= p 64))


;;;; Data types

(defun tiling-cfg (win point blessed) (list win point blessed))
(defun tiling-cfg-win (cfg) (car cfg))
(defun tiling-cfg-point (cfg) (cadr cfg))
(defun tiling-cfg-blessed (cfg) (caddr cfg))

(defvar tiling-configuration-list nil
  "Internal list for storing configurations.")

(defun tiling-cur-win () (tiling-cfg-win (car tiling-configuration-list)))
(defun tiling-cur-point () (tiling-cfg-point (car tiling-configuration-list)))
(defun tiling-cur-blessed () (tiling-cfg-blessed (car tiling-configuration-list)))

;;;; Internal
(defun tiling-current-is-activep ()
  (if tiling-configuration-list
      (compare-window-configurations
       (current-window-configuration)
       (tiling-cur-win))))

(defun tiling-set-cur-blessed (blessed)
  (setq tiling-configuration-list
        (cons (tiling-cfg (tiling-cur-win)
                          (point-marker)
                          blessed)
              (cdr tiling-configuration-list))))

;;;; Interface
(defun tiling-clear ()
  (interactive)
  (setq tiling-configuration-list nil))

(defun tiling-capture (&optional blessed)
  (interactive)
  (add-to-list 'tiling-configuration-list
               (tiling-cfg (current-window-configuration)
                           (point-marker)
                           blessed)))

(defun tiling-recapture (&optional blessed)
  (interactive)
  (if (not (null tiling-configuration-list))
      (pop-list-named 'tiling-configuration-list))
  (tiling-capture blessed))

(defun tiling-recapture-with-blessed ()
  (tiling-recapture (tiling-cur-blessed)))

(defun tiling-restore-current-cfg ()
  (interactive)
  (set-window-configuration (tiling-cur-win))
  ;; (goto-char (tiling-cur-point))
  )

(defun tiling-cycle-cfg ()
  (interactive)
  (if (and (> (length tiling-configuration-list) 1)
           (tiling-current-is-activep))
      (setq tiling-configuration-list
            (rotate-list tiling-configuration-list)
            ;; (cons (cadr tiling-configuration-list)
            ;;       (cons (car tiling-configuration-list)
            ;;             (cddr tiling-configuration-list)))
            ))
  (tiling-restore-current-cfg))

(defun tiling-bless-current-window ()
  (interactive)
  (tiling-set-cur-blessed
   (cons (car (window-list))
         (tiling-cur-blessed))))

(defun tiling-bless-clear-current ()
  (tiling-set-cur-blessed nil))

(defun tiling-switch-window ()
  (interactive)
  (let ((lst (tiling-cur-blessed)))
    (if t ;; (tiling-current-is-activep)
        (cond ((> (length lst) 1)
               (select-window (car lst))
               (tiling-set-cur-blessed (rotate-list lst)))
              (t
               (other-window 1)))
      (other-window 1))))

(defun tiling-cycle-or-recapture (p)
  (interactive "p")
  (cond ((triple-prefixp p)
         (tiling-clear)
         (message "Cleared all"))
        ((or (double-prefixp p) (null tiling-configuration-list))
         (tiling-capture)
         (message "Captured"))
        ((single-prefixp p)
         (tiling-recapture-with-blessed)
         (message "Recaptured"))
        (t
         (tiling-cycle-cfg))))

(defun tiling-switch-or-bless (p)
  (interactive "p")
  (cond ((double-prefixp p)
         (tiling-bless-clear-current)
         (tiling-bless-current-window)
         (message "Reblessed")
         (tiling-switch-window))
        ((single-prefixp p)
         (tiling-bless-current-window)
         (message "Blessed")
         (tiling-switch-window))
        (t
         (tiling-switch-window))))


;;;; Skip some modes automatically

(defvar tiling-skip-mode-list
  `(erc-mode
    rcirc-mode
    slime-repl-mode
    eshell-mode
    jabber-roster-mode
    jabber-chat-mode
    org-mode
    Info-mode
    help-mode))

(defvar tiling-skip-invert nil)

(defun tiling-skip-p (mode maybe-invert)
  (let ((skip (member mode tiling-skip-mode-list)))
    (if maybe-invert
        (not skip)
      skip)))

(defun tiling-find-other-window (origin maybe-invert)
  (other-window 1)
  (if (equal origin (current-window))
      (other-window 1)                  ; fallback to other-window
    (if (tiling-skip-p major-mode maybe-invert)
        (tiling-find-other-window origin maybe-invert)
     (if (tiling-current-is-activep)
          (tiling-recapture-with-blessed)))))

(defun tiling-find-main-window ()
  (interactive)
  (tiling-find-other-window (current-window) nil))

(defun tiling-find-skipped-window ()
  (interactive)
  (tiling-find-other-window (current-window) t))

;;; the old interface
(defun tiling-skip-other-window (toggle)
  (interactive "P")
  (if toggle (setq tiling-skip-invert (not tiling-skip-invert)))
  (tiling-find-other-window (current-window) tiling-skip-invert))

(defvar tiling-mode-map
  (easy-mmode-define-keymap
   (list (cons (kbd "C-<tab>") 'tiling-switch-or-bless)
         (cons (kbd "C-M-<tab>") 'tiling-cycle-or-recapture)
         ;; (cons (kbd "C-x o") 'tiling-skip-other-window)
         (cons (kbd "C-x o") 'tiling-find-main-window)
         (cons (kbd "C-c o") 'tiling-find-skipped-window))))

(define-minor-mode
  tiling-mode
  "Tiling window manager for emacs."
  :global t
  :init-value t
  :lighter nil
  :keymap tiling-mode-map)

(defalias 'capture-tiling 'tiling-capture)
(defalias 'recapture-tiling 'tiling-recapture)
(defalias 'clear-tiling 'tiling-clear)
(defalias 'bless-current-window 'tiling-bless-current-window)

(defun tiling-two-by-two ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally 80)
  (other-window 1)
  (split-window-horizontally 80)
  (other-window 1)
  (split-window-vertically))

(defun tiling-three-by-three ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally 80)
  (other-window 1)
  (split-window-horizontally 80)
  (other-window 1)
  (split-window-horizontally 80)
  (other-window 1)
  (split-window-vertically)
  (other-window 1)
  (split-window-vertically))

(defun tiling-two-up-one-down ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically -8)
  (split-window-horizontally))

(provide 'tiling)
