;;;; Utilities
(defun rotate-list (list-name)
  (append (cdr lst)
          (list (car lst))))

(defun pop-list-named (list-name)
  (set list-name
       (cdr (symbol-value list-name))))

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
  (compare-window-configurations (current-window-configuration)
                                 (tiling-cur-win)))

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

(defun tiling-restore-current-cfg ()
  (interactive)
  (set-window-configuration (tiling-cur-win))
  (goto-char (tiling-cur-point)))

(defun tiling-cycle-cfg ()
  (interactive)
  (if (and (> (length tiling-configuration-list) 1)
           (tiling-current-is-activep))
      (setq tiling-configuration-list
            (cons (cadr tiling-configuration-list)
                  (cons (car tiling-configuration-list)
                        (cddr tiling-configuration-list)))))
  (tiling-restore-current-cfg))

(defun tiling-bless-current-window ()
  (interactive)
  (tiling-set-cur-blessed
   (cons (car (window-list))
         (tiling-cur-blessed))))

(defun tiling-switch-window ()
  (interactive)
  (let ((lst (tiling-cur-blessed)))
    (if (not (and (> (length lst) 1)
                  (tiling-current-is-activep)))
        (progn
          ;; (message "other window")
          (other-window 1))
      (progn
        ;; (message "blessed")
        (select-window (car lst))
        (pop-list-named 'tiling-configuration-list)
        (tiling-capture (rotate-list lst))))))

(defvar tiling-mode-map
  (easy-mmode-define-keymap
   (list (cons (kbd "M-`") 'tiling-switch-window)
         (cons (kbd "C-<tab>") 'tiling-cycle-cfg))))

(define-minor-mode
  tiling-mode
  "Tiling window manager for emacs."
  :init-value t
  :lighter nil
  :keymap tiling-mode-map)

(defalias 'capture-tiling 'tiling-capture)
(defalias 'clear-tiling 'tiling-clear)
(defalias 'bless-current-window 'tiling-bless-current-window)

(provide 'tiling)
