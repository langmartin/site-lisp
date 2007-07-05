;;; User rc-* procs are at the bottom of this file.
;;;     the rest of this file is support functions.

(defalias 'qrr 'query-replace-regexp)
(defalias 'exit-emacs 'save-buffers-kill-emacs)
(defalias 'close-emacs 'save-buffers-kill-emacs)

(defvar rc-coptix-tab-width 4
  "The tab-width is a bit tricky to set, so this
variable is used in several hooks where a language mode resets the tab-width.
Set it intead of tab-width.")

(defun rc-coptix ()
  "Common initialization options for Coptix"
  (setq
   next-line-add-newlines nil
   require-final-newline t
   ring-bell-function 'ignore
   tab-width rc-coptix-tab-width
   standard-indent rc-coptix-tab-width
   indent-tabs-mode t
   ;; end sentences w/ just one space in text modes (french spacing
   sentence-end  "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"
   sentence-end-double-space nil
   ;; make backup files in ~/emacs~/ rather than scattered around all
   ;; over the filesystem.
   backup-by-copying t
   backup-directory-alist (cons '("." . "~/.emacs.d/backup/") backup-directory-alist)
   tramp-backup-directory-alist backup-directory-alist
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
  (setq auto-mode-alist
        (append '(("/tmp/mutt.*" . mail-mode))
		auto-mode-alist))

  (add-hook '2C-mode-hook
            '(lambda ()
               (setq 2C-window-width 24)
               (setq 2C-beyond-fill-column 0)
               (other-window 2)
               (setq fill-column 55)))
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (setq tab-width rc-coptix-tab-width)
               (setq c-basic-offset rc-coptix-tab-width)))
  (add-hook 'perl-mode-hook
            '(lambda ()
               (setq fill-column 78
                     perl-indent-level rc-coptix-tab-width)))
  (add-hooks '(lisp-mode-hook
               emacs-lisp-mode-hook
               scheme-mode-hook
               html-mode-hook
               sql-mode)
             '(lambda () (setq indent-tabs-mode nil)))
  (add-hooks '(text-mode-hook
               html-mode-hook
               text-mode-hook
               plain-TeX-mode-hook)
             'turn-off-auto-fill)
  (setq sh-basic-offset rc-coptix-tab-width)
  (setq css-indent-offset rc-coptix-tab-width)

  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key [f1] 'help-command)
  (global-set-key "OP" 'help-command)
  (global-unset-key [insert])

  ;;;; Home, End, Pgup and Pgdown
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
  (global-set-key [f5] 'call-last-kbd-macro)

  ;;; Load if it's there
  (require 'php-mode "php-mode" t)
  (add-to-list 'vc-handled-backends 'DARCS)
  (load "darcsum" t)
  (if (require 'css-mode "css-mode-fixed" t)
      (progn
        (setq auto-mode-alist
              (append
               '(("\\.css\\'" . css-mode))
               auto-mode-alist))
        (add-hook 'css-mode-hook
                  '(lambda ()
                     (setq css-indent-offset 4)))))
  (require 'html-helper-mode "html-helper-mode" t)
  (require 'visual-basic-mode "visual-basic-mode" t)
  (if (require 'javascript-mode "javascript-mode" t)
      (setq auto-mode-alist
	    (append
	     '(("\\.inc\\'" . javascript-mode)
	       ("\\.js\\'" . javascript-mode)
	       ("\\.asp\\'" . javascript-mode)
	       ("\\.asa\\'" . javascript-mode))
	     auto-mode-alist)))
  (rc-maybe-session)
  (rc-emacs22-only)
  (require 'http-twiddle "http-twiddle" t))

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

(defun rc-function-keys-mlm (key-fn)
  "Setup Lang's function keys"
  (interactive)
  (funcall key-fn [f3] 'kmacro-start-macro-or-insert-counter)
  (funcall key-fn [f4] 'kmacro-end-or-call-macro)
  (funcall key-fn [f5] 'call-last-kbd-macro)
  (funcall key-fn [f6] 'repeat)
  (funcall key-fn [f7] 'imenu)
  (funcall key-fn [end] 'point-to-register)
  (funcall key-fn [home] 'jump-to-register))

(defun cx-set-plain-tab-keys ()
  "Bind <tab> to always insert just a real tab"
  (interactive)
  (add-hook 'sh-mode-hook 'cx-dumb-tab)
  (setq tab-always-indent nil
        c-tab-always-indent nil
        perl-tab-always-indent nil))

(defun cx-dumb-tab ()
  "Set tab to just insert itself"
  (local-set-key "	" 'self-insert-command))

(defun rc-electric-keys ()
  "Electrify return and buffer lists"
  (global-set-key "\C-x\C-b" 'electric-buffer-list)
  (global-set-key "\C-m" 'newline-and-indent)
  (add-hook 'perl-mode-hook
            '(lambda () (local-set-key "\C-m" 'newline))))

(defun rc-maybe-gambit ()
  "Load and initialize Gambit-C scheme-mode extensions"
  (and (require 'gambit "gambit.el" t)
       (defun gambit-abort ()
	 "Return to top level. Equivalent to \",t\"."
	 (interactive)
	 (scheme-send-string ",t"))
       (add-hook 'scheme-mode-hook
		 (lambda () (local-set-key "\C-cx" 'gambit-abort)))))

(defun rc-maybe-session ()
  (if (require 'session "session.el" t)
      (session-initialize)))

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




;;;; Unstable
(defun rc-maybe-mmm-mode ()
  (interactive)
  (if (require 'mmm-auto "mmm-mode" t)
      (progn
        (setq mmm-global-mode 'maybe)
        (mmm-add-classes
         '((asp-javascript
            :submode javascript-mode
            :face mmm-code-submode-face
            :front "<%"
            :back "%>")
           (asp-vbscript
            :submode visual-basic-mode
            :face mmm-code-submode-face
            :front "<%"
            :back "%>")))
        (define-derived-mode asp-mode html-mode "ASP/HTML"))))

(defun rc-asp-javascript-mode ()
  "Broken still"
  (interactive)
  (rc-maybe-mmm-mode)
  (add-to-list 'auto-mode-alist '("\\.as[ap]\\'" . asp-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . asp-mode))
  (add-to-list 'mmm-mode-ext-classes-alist '(asp-mode nil html-js))
  (add-to-list 'mmm-mode-ext-classes-alist '(asp-mode nil embedded-css))
  (add-to-list 'mmm-mode-ext-classes-alist '(asp-mode nil asp-javascript)))



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

(defun comint-send-C-something (char)
  (interactive)
  (comint-send-input t t)
  (comint-send-string
   (get-buffer-process (current-buffer))
   char))
(defun comint-send-C-c ()
  (interactive)
  (comint-send-C-something ""))
(defun comint-send-C-z ()
  (interactive)
  (comint-send-C-something ""))
(add-hook
 'shell-mode-hook
 (lambda ()
   (local-set-key "\C-c\C-c" 'comint-send-C-c))
 (lambda ()
   (local-set-key "\C-c\C-z" 'comint-send-C-z)))

(defun local-shell ()
  (interactive)
  (shell "*local*"))
(defun iago-shell ()
  (interactive)
  (shell "*iago*"))
(defun hook-shell ()
  (interactive)
  (shell "*hook*"))
(defun volt-shell ()
  (interactive)
  (shell "*volt*"))


;;;; Growl
(defun growl (title message)
  (start-process "growl" " growl"
                 "growlnotify"
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))
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

(global-set-key "\C-w" 'kill-backward-word-or-region)

(defun cx-build-tags ()
  "Create a tags table in the top of your darcs project."
  (interactive)
  (cx-build-tags-primitive (lambda () (file-exists-p "_darcs"))))

(defun cx-build-tags-cvs ()
  "Create a tags table in the top of your CVS project."
  (interactive)
  (cx-build-tags-primitive (lambda () (not (file-exists-p "../CVS")))))

(defun cx-build-tags-primitive (top-dir-pred)
  "Create a tags table in the top of your project"
  (let ((dir (cx-find-top-dir top-dir-pred)))
    (message "top directory here is %s" dir)
    (if (string-equal dir "/")
        (message "Can't find a _darcs directory in this path")
      (shell-command
       (concat
        "find . -type f"
        "| grep -v -e '_darcs/' -e 'CVS/'"
        "| grep -v -f /coptix/admin/scripts/share/etags-grep-anti-patterns.txt"
        "| grep -f /coptix/admin/scripts/share/etags-grep-patterns.txt"
        "| tr '\\n' '\\0'"
        "| xargs -0 etags"
        )))))

(defun cx-find-top-dir (pred)
  "Find the top directory containing the name contains, bail out at the top"
  (if (or (funcall pred)
          (= (nth 10 (file-attributes "."))
             (nth 10 (file-attributes ".."))))
      (file-truename ".")
    (progn
      (cd "..")
      (cx-find-top-dir pred))))

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

(add-to-list 'auto-mode-alist '("\\.psql$" . sql-mode))
(defun customize-sql-mode-postgres ()
  (turn-on-font-lock)
  (cond
   ((string= (file-name-extension (buffer-file-name)) "psql")
    (progn
      (sql-highlight-postgres-keywords)
      (local-set-key "\C-c!" 'sql-postgres)))))
(add-hook 'sql-mode-hook 'customize-sql-mode-postgres)




;;; User init functions
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
  (setq gambit-highlight-color "gray")
  (rc-maybe-gambit)
  (rc-electric-keys)
  (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
  (load "modal-emacs.el" t))

(defun rc-james ()
  "James Long: rc-schemers + VIper + electric everything"
  (interactive)
  (rc-schemers)
  (rc-viper))

(defun rc-lang ()
  "Lang Martin: rc-schemers + electric everything"
  (interactive)
  (setq rc-coptix-tab-width 8)
  (rc-schemers)
  (global-set-key "\M-/" 'hippie-expand)
  (global-set-key "\C-x\C-b" 'buffer-menu)
  (rc-function-keys-mlm 'global-set-key))

(defun rc-d ()
  "Andy Montgomery: rc-schemers + hanging braces"
  (interactive)
  (rc-schemers)
  (add-hook 'c-mode-hook
            '(lambda ()
               (c-set-style "bsd"))))
