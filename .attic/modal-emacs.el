;; (global-set-key [f2] 'modal-emacs)
;; (global-set-key "\e\C-m" 'modal-emacs)
;; (modal-emacs-init-skip)x

(defun foldr (proc seed lst)
  (if (null lst)
      seed
    (funcall proc (car lst)
             (foldr proc
                    seed
                    (cdr lst)))))

(defun modal-insert (next-proc)
  "Exit modal and perform function"
  `(lambda (&optional arg)
     (interactive)
     (call-interactively ',next-proc)
     (modal-emacs 0)))

(defun modal-insert-mode ()
  "Exit modal"
  (interactive)
  (modal-emacs 0))

(defun quick-search-forward (arg)
  "Search for a char like vi"
  (interactive "p")
  (let ((c (read-char "Look for: " nil)))
    (search-forward (char-to-string c) nil nil arg)
    (backward-char 1)))

(defun quick-search-backward (arg)
  "Search for a char like vi"
  (interactive "p")
  (let ((c (read-char "Look for: " nil)))
    (search-backward (char-to-string c) nil nil arg)))

(defun mark-and-search-forward ()
  (interactive)
  (push-mark)
  (call-interactively 'isearch-forward-regexp))

(defun mark-and-search-backward ()
  (interactive)
  (push-mark)
  (call-interactively 'isearch-backward-regexp))

(defvar modal-emacs-map nil)
(setq modal-emacs-map
      (let ((map (make-sparse-keymap)))
        (define-key map "0" 'digit-argument)
        (define-key map "1" 'digit-argument)
        (define-key map "2" 'digit-argument)
        (define-key map "3" 'digit-argument)
        (define-key map "4" 'digit-argument)
        (define-key map "5" 'digit-argument)
        (define-key map "6" 'digit-argument)
        (define-key map "7" 'digit-argument)
        (define-key map "8" 'digit-argument)
        (define-key map "9" 'digit-argument)
        (define-key map "-" 'negative-argument)

        (define-key map "a" 'beginning-of-line)
        (define-key map "A" (modal-insert 'back-to-indentation))
        (define-key map "b" 'backward-char)
        (define-key map "B" 'backward-word)
        (define-key map "c" 'mode-specific-command-prefix)
        (define-key map "d" 'delete-char)
        ;;(define-key map "D" 'kill-word)
        (define-key map "e" 'end-of-line)
        (define-key map "E" (modal-insert 'end-of-line))
        (define-key map "f" 'forward-char)
        (define-key map "F" 'forward-word)
        (define-key map "w" 'forward-word)
        (define-key map "n" 'next-line)
        (define-key map "p" 'previous-line)
        (define-key map "i" 'modal-insert-mode)
        (define-key map "I" 'imenu)
        (define-key map "k" 'kill-line)
        (define-key map "K" 'kill-sexp)
        (define-key map "o" (modal-insert 'open-line))
        (define-key map "t" 'transpose-chars)
        (define-key map "T" 'transpose-sexps)
        (define-key map "v" 'scroll-up)
        (define-key map "V" 'scroll-down)
        (define-key map "q" 'quoted-insert)
        (define-key map "y" 'yank)
        (define-key map "Y" 'yank-pop)
        (define-key map "x" 'ctl-x-map)
        (define-key map "X" 'execute-extended-command)
        (define-key map "z" 'zap-to-char)
        (define-key map "^" 'delete-indentation)

        (define-key map "m" 'point-to-register)
        (define-key map "j" 'jump-to-register)

        (define-key map "s" 'isearch-forward)
        (define-key map "r" 'isearch-backward)
        (define-key map "u" 'undo)
        (define-key map "_" 'undo)
        (define-key map "/" 'undo)
        (define-key map " " 'set-mark-command)
        (define-key map "g" (key-binding "\eg"))
        (define-key map "l" 'recenter)
        (define-key map "!" 'shell-command)
        (define-key map "|" 'shell-command-on-region)
        (define-key map "\\" 'delete-horizontal-space)
        (define-key map "=" 'indent-region)
        (define-key map ";" (modal-insert 'comment-dwim))
        (define-key map "'" 'abbrev-prefix-mark)

        (define-key map "}" 'forward-paragraph)
        (define-key map "{" 'backward-paragraph)
        (define-key map "]" 'forward-sexp)
        (define-key map "[" 'backward-sexp)
        (define-key map "(" 'kmacro-start-macro)
        (define-key map ")" 'kmacro-end-macro)
        (define-key map "." 'quick-search-forward)
        (define-key map "," 'quick-search-backward)
        (define-key map "<" 'beginning-of-buffer)
        (define-key map ">" 'end-of-buffer)
        map))

(defcustom modal-emacs nil
  "If non-nil, modal emacs is in effect. If you customize
it to be t, then modal emacs will start by default."
  :group 'modal-emacs
  :type 'boolean)

(make-variable-buffer-local 'modal-emacs)

(defun modal-emacs (&optional arg)
  "Enter modal editing mode (like having C- pressed)"
  (interactive "p")
  (setq modal-emacs (> arg 0))
  (force-mode-line-update))

(add-to-list 'minor-mode-alist '(modal-emacs " Modal"))

(setq minor-mode-map-alist
      (foldr '(lambda (x acc)
                (if (eq 'modal-emacs (car x))
                    acc
                  (cons x acc)))
             nil
             minor-mode-map-alist))

(add-to-list 'minor-mode-map-alist
             (cons 'modal-emacs
                   modal-emacs-map))

(defcustom modal-emacs-skip-modes
  '(custom-mode-hook
    debugger-mode-hook
    electric-buffer-menu-mode-hook
    erc-mode-hook
    Info-mode-hook
    minibuffer-setup-hook
    view-mode-hook)
  "A list of mode hooks which should be used to turn off
modal emacs. Probably only useful if you have it turned on
by default."
  :type '(repeat variable)
  :group 'modal-emacs)

(defun modal-emacs-init-skip ()
  (mapcar '(lambda (x)
             (add-hook x 'modal-insert-mode))
          modal-emacs-skip-modes))

;; (modal-emacs-init-skip)

(provide 'modal-emacs)
