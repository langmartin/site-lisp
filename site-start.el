;;; User rc-* procs are at the bottom of this file.
;;;     the rest of this file is support functions.

(defalias 'qrr 'query-replace-regexp)
(defalias 'exit-emacs 'save-buffers-kill-emacs)
(defalias 'close-emacs 'save-buffers-kill-emacs)

(defvar rc-coptix-tab-width 4
  "* The tab-width is a bit tricky to set, so this
variable is used in several hooks where a language mode resets the tab-width.
Set it intead of tab-width.")

(defun rc-coptix ()
  "Common initialization options for Coptix"
  (custom-set-variables
   `(tab-width ,rc-coptix-tab-width)
   `(c-basic-offset ,rc-coptix-tab-width)
   `(standard-indent ,rc-coptix-tab-width)
   `(perl-indent-level ,rc-coptix-tab-width)
   `(sh-basic-offset ,rc-coptix-tab-width)
   `(css-indent-offset ,rc-coptix-tab-width))
  (setq
   next-line-add-newlines nil
   require-final-newline t
   ring-bell-function 'ignore
   indent-tabs-mode t
   ;; end sentences w/ just one space in text modes (french spacing
   sentence-end  "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"
   sentence-end-double-space nil
   ;; make backup files in ~/emacs~/ rather than scattered around all
   ;; over the filesystem.
   backup-by-copying t
   backup-directory-alist (cons '("." . "~/.emacs.d/backup/") backup-directory-alist)
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
  (add-to-list 'auto-mode-alist '("/tmp/mutt.*" . mail-mode))

  (add-hook '2C-mode-hook
            '(lambda ()
               (setq 2C-window-width 24)
               (setq 2C-beyond-fill-column 0)
               (other-window 2)
               (setq fill-column 55)))
  (add-hooks '(lisp-mode-hook
               emacs-lisp-mode-hook
               scheme-mode-hook
               html-mode-hook
               sql-mode)
             '(lambda () (setq indent-tabs-mode nil)))
  (add-hooks '(html-mode-hook)
             'turn-off-auto-fill)

  (global-set-key "\C-h" 'delete-backward-char) 
  (global-set-key "\M-h" 'help)         ; was mark-paragraph
  (global-set-key [f1] 'help)
  (global-set-key "OP" 'help)
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
  ;; (add-to-list 'vc-handled-backends 'DARCS)
  ;; (load "darcsum" t)
  (defun rc-broken-version-of-css-mode-so-please-dont ()
    (if (require 'css-mode "css-mode-fixed" t)
        (progn
          (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
          (add-hook 'css-mode-hook
                    '(lambda ()
                       (setq css-indent-offset 4))))))
  ;; (require 'html-helper-mode "html-helper-mode" t)
  (require 'visual-basic-mode "visual-basic-mode" t)
  (rc-maybe-session)
  (rc-emacs22-only)
  (require 'http-twiddle "http-twiddle" t))

(require 'rst)                          ; defines a slow, recursive filter

(defun filter (proc lst)
  (let ((value nil))
    (while lst
      (if (funcall proc (car lst))
          (setq value (cons (car lst) value)))
      (setq lst (cdr lst)))
    value))

(defun rc-javascript-auto-mode-alist (mode)
  (let ((files `("\\.js\\'" "\\.inc\\'" "\\.asp\\'" "\\.asa\\'")))
    (setq auto-mode-alist
          (filter (lambda (x)
                    (not (member (car x) files)))
                  auto-mode-alist))
    (mapc (lambda (x)
            (setq auto-mode-alist
                  (cons (cons x mode)
                        auto-mode-alist)))
          files)))

(defun rc-c-like-javascript-mode-which-crashes-emacs ()
  (require 'javascript-mode "javascript")
  (rc-javascript-auto-mode-alist 'javascript-mode))

(defun rc-js2-javascript-mode ()
  (interactive)
  (require 'js2-mode)
  (custom-set-variables
   (list 'js2-basic-offset rc-coptix-tab-width))
  (rc-javascript-auto-mode-alist 'js2-mode))

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
  "bind some function keys [kp-[01enter]], C-xC-h, [home]"
  (interactive)
  (global-set-key [kp-1] 'imenu)
  (global-set-key [super-1] 'imenu)
  (global-set-key [kp-0] 'repeat)
  (global-set-key [super-0] 'repeat)
  (global-set-key [kp-enter] 'help)
  (global-set-key [super-enter] 'help)
  (funcall key-fn [home] 'jump-to-register)
  (add-hooks '(erc-mode-hook)
             '(lambda () (local-set-key [home] 'jump-to-register))))

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
  (add-hooks '(perl-mode-hook python-mode-hook)
	     '(lambda () (local-set-key "\C-m" 'newline))))

;;;; scheme setup
(defun gambit-setup ()
  "Gambit-C scheme-mode extensions"
  (interactive)
  (setq gambit-highlight-color "gray")
  (if nil (progn
            (require 'gambit "gambit.el" t)
            (defun gambit-abort ()
              (interactive)
              (scheme-send-string ",t")))
    (defun gambit-abort ()
      (interactive)
      (comint-send-string (scheme-proc) ",t\n")))
  (add-hook 'scheme-mode-hook
            (lambda () (local-set-key "\C-cx" 'gambit-abort)))
  (setq scheme-program-name "gsi -:d-")
  (scheme-extend-info "(gambit-c)General Index"))

(defun scheme48-setup ()
  "Scheme48 scheme-mode extensions"
  (interactive)
  (require 'scheme48)
  (setq scheme-program-name "scheme48")
  (scheme-extend-info "(scheme48)Binding Index"))

(defun rc-paredit ()
  (require 'paredit)
  (add-hooks '(scheme-mode-hook
               emacs-lisp-mode-hook
               lisp-mode-hook)
             '(lambda () (paredit-mode +1))
             'append))

(defun scheme-add-keywords (keyword-rules)
   (let* ((keyword-list (mapcar #'(lambda (x)
                                    (symbol-name (car x)))
                                keyword-rules))
          (keyword-regexp (concat "(\\("
                                  (regexp-opt keyword-list)
                                  "\\)[ \n]")))
     (font-lock-add-keywords 'scheme-mode
                             `((,keyword-regexp 1 font-lock-keyword-face))))
   (scheme-add-indentations keyword-rules))

(load "scheme-indent.el")

(defun scheme-add-indentations (rules)
  (mapc #'(lambda (x)
            (put (car x)
                 'scheme-indent-function
                 (cadr x)))
        rules))

(scheme-add-keywords
 '((when 1)
   (unless 1)
   ))

(scheme-add-indentations
 '((and-let* 1)
   (let-optionals 2)
   (let-optionals* 2)
   (let-port-rest 2)
   (wind-fluid 3)
   (case-equal 1)
   (let-list 1)
   (let-fluid 2)
   (let-sxml-pluck-attrs 2)
   (let-sxml-attrs 2)
   (case-regex 1)
   (case-posix-regex 1)
   (with-form-data 1)
   (let-conversion 1)
   (sxml-convert 1)
   
   (let-fluids with-...)
   (call-with-values 0)
   (let-unspec* 2)

   (run 1)
   (run* 1)
   (fresh 1)
   (conde 0)
   (match 1)
   (receive 2)
   
   ;; ykk-ports
   (with-current-output-port 1)
   (let-current-output-port 1)
   (maybe-current-output-port 1)
   (let-maybe-current-output-port 1)
   (with-current-input-port 1)
   (let-current-input-port 1)
   (maybe-current-input-port 1)
   (let-maybe-current-input-port 1)
   (with-string-input-port 1)
   (let-string-input-port 1)
   (with-string-ports 1)
   (let-string-ports 1)

   ;; ykk http
   (let-http-response 1)
   (let-headers 1)
   (output-response 2)

   ;; schim
   (with-package 1)
   (with-tag-set 1)
   (with-item 1)
   ))

(defun scheme-extend-info (page)
 (require 'info-look)
 (info-lookup-add-help
  :mode 'scheme-mode
  :regexp "[^()`',\" \t\n]+"
  :ignore-case t
  :doc-spec `(("(r5rs)Index" nil "^[ \t]+-+ [^:]+:[ \t]*" "\\b")
              (,page))))

;;;; rc-* setup functions to manage some bits
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

(defun rc-screen-ify-control-t (set-key)
  "Set a C-t map of buffer commands to make things work a bit
like GNU screen with a C-t command key."
  (funcall set-key "\C-t" nil)
  (funcall set-key "\C-tt" 'transpose-chars)
  (funcall set-key "\C-t\C-t" 'iswitchb-buffer)
  (funcall set-key "\C-t\C-i" 'other-window)
  (funcall set-key "\C-tn" 'next-buffer)
  (funcall set-key "\C-t\C-n" 'next-buffer)
  (funcall set-key "\C-tp" 'previous-buffer)
  (funcall set-key "\C-t\C-p" 'previous-buffer)
  (funcall set-key "\C-tS" 'split-window-vertically)
  (funcall set-key "\C-tX" 'delete-window)
  ;; these are kind of my own riff on screen commands
  (funcall set-key "\C-tr" 'iswitchb-display-buffer)
  ;; these are like vi's f
  ;; (funcall set-key "\C-tf" 'mark-and-search-forward)
  ;; (funcall set-key "\C-tb" 'mark-and-search-backward)
  )

;;;; Growl
(defun growl (title message)
  (start-process "growl" " growl"
                 "growlnotify"
                 title
                 "-a" "Emacs.app")
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

;; this turns out to be bad, since it needs to be mapped in lots of places
;; (global-set-key "\C-w" 'kill-backward-word-or-region)

(defun cx-build-tags ()
  "Create a tags table in the top of your darcs project."
  (interactive)
  (cx-build-tags-primitive
   (lambda () (or (file-exists-p "_darcs")
                  (file-exists-p ".git")))))

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
  (scheme48-setup)
  (rc-paredit)
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
  (global-set-key "\M-/" 'hippie-expand)
  (rc-function-keys-mlm 'global-set-key)
  (setq truncate-lines t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'reverse)
  (rc-js2-javascript-mode)
  )

(defun rc-d ()
  "Andy Montgomery: rc-schemers + hanging braces"
  (interactive)
  (rc-schemers)
  (add-hook 'c-mode-hook
            '(lambda ()
               (c-set-style "bsd"))))
