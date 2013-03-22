;; -*- no-byte-compile: t -*-

;;; User rc-* procs are at the bottom of this file.
;;;     the rest of this file is support functions.

(require 'cl)
(require 'utility)
(require 'srfi-1)

(defalias 'qrr 'query-replace-regexp)
(defalias 'exit-emacs 'save-buffers-kill-emacs)
(defalias 'quit-emacs 'save-buffers-kill-emacs)

(defvar rc-coptix-tab-width 4
  "* The tab-width is a bit tricky to set, so this
variable is used in several hooks where a language mode resets the tab-width.
Set it intead of tab-width.")

(defun rc-coptix ()
  "Common initialization options for Coptix"
  (set-variables
   `(tab-width ,rc-coptix-tab-width)
   `(c-basic-offset ,rc-coptix-tab-width)
   `(standard-indent ,rc-coptix-tab-width)
   `(perl-indent-level ,rc-coptix-tab-width)
   `(sh-basic-offset ,rc-coptix-tab-width)
   `(css-indent-offset ,rc-coptix-tab-width))
  (setq
   next-line-add-newlines nil
   require-final-newline t
   ;; ring-bell-function 'ignore
   indent-tabs-mode t
   ;; end sentences w/ just one space in text modes (french spacing
   sentence-end  "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"
   sentence-end-double-space nil
   ;; version control settings
   vc-follow-symlinks t
   vc-suppress-confirm t
   vc-cvs-stay-local nil
   ;; other junk
   font-lock-maximum-decoration nil
   server-temp-file-regexp "^/tmp/mutt\\|draft\\|Re"
   ispell-program-name "aspell"
   ispell-local-dictionary "american"
   tex-default-mode 'plain-tex-mode
   tex-run-command "pdfetex")

  (show-paren-mode 1)
  (transient-mark-mode 1)
  (global-font-lock-mode 1)
  (add-to-auto-mode-alist '(("/tmp/mutt.*" . mail-mode)
                            ("mail\\.google\\.com.*" . mail-mode)))

  (setq auto-mode-alist
        (filter (lambda (pair)
                  (if (eq (cdr pair) 'conf-mode-maybe)
                      nil
                    pair))
                auto-mode-alist))

  (add-hook '2C-mode-hook
            #'(lambda ()
               (setq 2C-window-width 24)
               (setq 2C-beyond-fill-column 0)
               (other-window 2)
               (setq fill-column 55)))

  (defun turn-off-indent-tabs-mode ()
    (setq indent-tabs-mode nil))

  (add-hooks '(lisp-mode-hook
               emacs-lisp-mode-hook
               scheme-mode-hook
               html-mode-hook
               sql-mode)
             'turn-off-indent-tabs-mode)
  (add-hooks '(html-mode-hook)
             'turn-off-auto-fill)

  (defun rc-C-h-delete-M-h-help ()
    (global-set-key "\C-h" 'backward-delete-char-untabify)
    (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
    (define-key paredit-mode-map "\C-h" 'paredit-backward-delete)
    (global-set-key "\M-h" 'help)       ; was mark-paragraph
    (global-set-key [f1] 'help)
    (global-set-key "OP" 'help))

  (global-unset-key [insert])

  (defun rc-home/end/etc-terminal-bindings ()
    "Home, End, Pgup and Pgdown bindings for terminals. I don't
use this anymore and finally ran into a key conflict with it, so
I'm skipping over it for now."
    (global-set-key [home] 'beginning-of-line)
    (global-set-key "\e[7~" 'beginning-of-line)
    (global-set-key "[H" 'beginning-of-line)
    (global-set-key "OH" 'beginning-of-line)
    (global-set-key [C-home] 'beginning-of-buffer)
    (global-set-key "\e[7^" 'beginning-of-buffer)
    (global-set-key "[5D" 'beginning-of-buffer)
    (global-set-key "[1;5D" 'beginning-of-buffer)
    (global-set-key [end] 'end-of-line)
    (global-set-key [select] 'end-of-line)
    (global-set-key "\e[8~" 'end-of-line)
    (global-set-key "[F" 'end-of-line)
    (global-set-key "OF" 'end-of-line)
    (global-set-key [C-end] 'end-of-buffer)
    (global-set-key [C-select] 'end-of-buffer)
    (global-set-key "\e[8^" 'end-of-buffer)
    (global-set-key "[5C" 'end-of-buffer)
    (global-set-key "[1;5C" 'end-of-buffer)
    (global-set-key [C-next] 'scroll-other-window)
    (global-set-key "\e[6^" 'scroll-other-window)

    ;;(global-set-key "OB" 'scroll-other-window)      ; Terminal.app arrows
    (global-set-key "[1;5B" 'scroll-other-window)
    (global-set-key "[6;5~" 'scroll-other-window)
    (global-set-key [C-prior] 'scroll-other-window-down)
    (global-set-key "\e[5^" 'scroll-other-window-down)
    ;;(global-set-key "OA" 'scroll-other-window-down) ; Terminal.app arrows
    (global-set-key "[1;5A" 'scroll-other-window-down)
    (global-set-key "[5;5~" 'scroll-other-window-down)
    (global-set-key [f5] 'call-last-kbd-macro))

  ;; Load if it's there
  (require 'php-mode "php-mode" t)

  ;; (require 'html-helper-mode "html-helper-mode" t)
  (require 'visual-basic-mode "visual-basic-mode" t)
  (rc-emacs22-only)
  (require 'http-twiddle "http-twiddle" t)
  (require 'make-tags-file))

(defun rc-backups-and-autosave-directory (backup)
  "Set all the variables to move backups & autosave files out of
the working directory"
  (let ((backup (if (eql "/" (aref backup (- (length backup) 1)))
                    backup
                  (concat backup "/"))))
   (make-directory backup t)
   (setq backup-by-copying t
         delete-old-versions t
         kept-new-versions 10
         kept-old-versions 2
         version-control t
         backup-directory-alist `(("." . ,backup))
         tramp-backup-directory-alist backup-directory-alist
         auto-save-list-file-prefix (concat backup ".auto-saves-")
         auto-save-file-name-transforms `((".*" ,backup t)))))

(rc-backups-and-autosave-directory "~/.emacs.d/backup")

(progn
  (require 'rst)                    ; defines a slow, recursive filter
  (add-hook 'rst-mode-hook 'turn-on-auto-fill))

(defun rc-emacs22-only ()
  (and (string-match "Emacs 22" (emacs-version))
       (progn
         (load "csv-nav" t))))

(defun rc-utf8 ()
  "Setup terminal for UTF-8"
  (interactive)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(defun rc-coptix-keybindings ()
  "Keybindings for Cygwin exit and mark and for goto-line"
  (interactive)
  ;;; Work around term=cygwin
  (global-set-key "\C-x\C-e" 'save-buffers-kill-emacs)
  (global-set-key "\C-o" 'set-mark-command)
  (global-set-key "\C-xt" 'cx-datetime)
  (global-set-key "\C-xc" 'cx-comment)
  (global-set-key "\C-x+" 'goto-line))

(defun rc-function-keys-mlm (&optional key-fn)
  "bind some function keys [kp-[01enter]], C-xC-h, [home]"
  (interactive)
  (global-set-key (kbd "H-i") 'imenu)
  (global-set-key (kbd "H-r") 'repeat)
  (global-set-key (kbd "H-h") 'help)
  (global-set-key [home] 'jump-to-register)
  (add-hook 'erc-mode-hook
            #'(lambda () (local-set-key [home] 'jump-to-register))))

(defun cx-set-plain-tab-keys ()
  "Bind <tab> to always insert just a real tab"
  (interactive)
  (add-hook 'sh-mode-hook 'cx-dumb-tab)
  (setq tab-always-indent nil
        c-tab-always-indent nil
        perl-tab-always-indent nil))

(defun cx-dumb-tab ()
  "Set tab to just insert itself"
  (local-set-key "      " 'self-insert-command))

(defun rc-electric-keys ()
  "Electrify return and buffer lists"
  (global-set-key "\C-x\C-b" 'electric-buffer-list)
  (global-set-key "\C-m" 'newline-and-indent)
  (add-hooks '(perl-mode-hook python-mode-hook)
             #'(lambda () (local-set-key "\C-m" 'newline))))

(require 'scheme-rc)

;;;; rc-* setup functions to manage some bits
(defvar rc-viper-ESC-key "\C-z"
  "See viper-ESC-key. This one is used by rc-viper.")

(defun rc-viper ()
  (setq viper-mode t)
  ;; these next two are alternatives. Using a different key works better.
  (setq viper-ESC-key rc-viper-ESC-key)
  ;; (setq viper-ESC-keyseq-timeout 0 viper-no-multiple-ESC t viper-translate-all-ESC-keysequences nil)
  (setq viper-ex-style-motion nil viper-ex-style-editing nil)
  (require 'viper)
  (require 'advice)
  (defadvice viper-undo (around viper-undo-more-maybe activate)
    "Have `viper-undo' behave more like Vim."
    (if (eq last-command 'viper-undo)
        (viper-undo-more)
      ad-do-it))
  (defun viper-adjust-undo ()
    "Redefined to empty function so that movement commands with cursor break the undo list"))

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows. See:
http://www.emacswiki.org/cgi-bin/wiki/TransposeWindows"
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(define-key ctl-x-4-map "\C-t" 'transpose-windows)

(defun toggle-vertical-horizontal-window-split ()
  "In a frame with two windows, toggle between vertical and
horizontal split. See:
http://www.emacswiki.org/cgi-bin/wiki/ToggleWindowSplit"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "v" 'toggle-vertical-horizontal-window-split)

;;;; Growl
(defun growl (title message)
  (let ((proc (start-process "growl"
                             "*growl*"
                             "growlnotify"
                             title "-a" "Emacs.app")))
    (process-send-string proc message)
    (process-send-string proc "\n")
    (process-send-eof proc)))

(defun erc-growl (nick message)
  (let ((n (substring nick 0 (string-match "\\!" nick))))
    (growl "ERC" (format "%s said %s" n message))
    nil))

(defun erc-growl-match (match-type nick message)
  (when (and ;; I don't want to see anything from the erc server
             (null (string-match "\\`\\([sS]erver\\|localhost\\)" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick)))
    (erc-growl nick message)))

;;;; Utility Functions
(defun cx-date () "Insert a date stamp in coptix format"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun cx-date-pretty ()
  (interactive)
  (insert (format-time-string "%a, %b %d %Y")))

(defun cx-time () "Insert a time stamp in coptix format"
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun cx-datetime ()
  "Insert time and date as %Y-%m-%d %H:%M:%S"
  (interactive)
  (cx-date)
  (insert " ")
  (cx-time))

(defun cx-comment ()
  "Insert the coptix shell comment block"
  (interactive)
  (insert-file "/coptix/admin/scripts/share/template/comment.sh"))

(defun run-scheme-remote (server)
  "Run scheme on a remote server"
  (interactive "s Server: ")
  (run-scheme (concat "ssh " server " gsi -:d-")))

(defun unfill-paragraph (from to &optional arg)
  "Unfill current paragraph.
If invoked with a non-nil prefix argument or in Transient Mark mode
when the mark is active unfill the region as paragraphs instead. When
called from lisp FROM and TO determine the region to unfill. When
repeated unfill entire region as one paragraph."
  (interactive (list (and (mark) (region-beginning))
                     (and (mark) (region-end))
                     current-prefix-arg))
  (let ((fill-column (point-max)))
    (if (or (not (interactive-p))
            arg
            (eq last-command 'unfill-paragraph-partial)
            (and transient-mark-mode mark-active))
        (if (eq last-command 'unfill-paragraph-partial)
            (fill-region-as-paragraph from to)
          (fill-nonuniform-paragraphs from to)
          (setq this-command 'unfill-paragraph-partial))
      (fill-paragraph nil))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key "\C-x\M-e" 'eval-and-replace)

(defun kill-backward-word-or-region (&optional arg)
  "This function provides the one VI key that I absolutely can't stop hitting"
  (interactive)
  (if (null mark-active)
      (backward-kill-word (prefix-numeric-value arg))
    (kill-region (region-beginning) (region-end))))

;; this turns out to be bad, since it needs to be mapped in lots of places
;; (global-set-key "\C-w" 'kill-backward-word-or-region)

(defun doctype-xhtml ()
  (interactive)
  (insert
   "<!DOCTYPE html
       PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

(defun doctype-strict ()
  (interactive)
  (insert
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
            \"http://www.w3.org/TR/html4/strict.dtd\">"))

(defun add-hooks (hooks function &optional append local)
  (mapcar (lambda (x)
            (add-hook x function append local))
          hooks))

(add-to-auto-mode-alist '(("\\.psql$" . sql-mode)))

(defun customize-sql-mode-postgres ()
  (turn-on-font-lock)
  (cond
   ((and (buffer-file-name)
         (string= (file-name-extension (buffer-file-name)) "psql"))
    (progn
      (sql-highlight-postgres-keywords)
      (local-set-key "\C-c!" 'sql-postgres)))))
(add-hook 'sql-mode-hook 'customize-sql-mode-postgres)

(defun ack (command)
  "Run ack like grep"
  (interactive
   (list
    (read-from-minibuffer
     "Run ack (-n: no recurse, -a: all filetypes, -i: case insesitive) "
     "ack --nocolor --nogroup -H")))
  (grep command))

;;;; User init functions
(defun rc-ben ()
  "Ben Huffine: coptix + nothing"
  (interactive)
  (rc-coptix))

(defun rc-jeffrey ()
  "Jeffrey Cross: coptix + plain tab key"
  (interactive)
  (rc-coptix)
  (rc-coptix-keybindings)
  (cx-set-plain-tab-keys))

(defun rc-schemers ()
  "Collect a few of the semi-standard initialization options"
  (rc-coptix)
  (rc-electric-keys)
  ;; (rc-paredit)
  (iswitchb-mode t))

(defun rc-james ()
  "James Long: rc-schemers + electric everything"
  (interactive)
  (rc-schemers))

(defun rc-lang ()
  "Lang Martin: rc-schemers + electric everything"
  (interactive)
  (setq rc-coptix-tab-width 8)
  (rc-schemers)
  (global-set-key "\C-x\C-b" 'buffer-menu)
  (progn
    (global-set-key (kbd "M-/") 'hippie-expand)
    (setq hippie-expand-try-functions-list
          '(try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol)))
  ;; (rc-function-keys-mlm 'global-set-key)
  (fset 'yes-or-no-p 'y-or-n-p)
  (require 'uniquify nil t)
  ;; (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-strip-common-suffix t))

(defun rc-d ()
  "Andy Montgomery: rc-schemers + hanging braces"
  (interactive)
  (rc-schemers)
  (add-hook 'c-mode-hook
            #'(lambda ()
               (c-set-style "bsd"))))
