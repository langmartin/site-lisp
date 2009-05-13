(load "site-start")
(rc-lang)

(require 'js2-mode-rc)

;; things for cocoa emacs 23
(progn
  (require 'mwheel)
  (global-set-keys
   '(("<wheel-down>" . mwheel-scroll)
     ("<wheel-up>"   . mwheel-scroll)
     ("M-`"          . other-frame)))
  (require 'tramp)
  (setq tramp-default-proxies-alist
        `(("localhost" "\\`root\\'" nil)
          (nil "\\`root\\'" "/ssh:%h:")))
  (mapc (lambda (x)
          (add-to-list 'tramp-default-method-alist
                       `(,x nil "sshx")))
        '("rove" "abla" "drok"))
  (setq tramp-backup-directory-alist backup-directory-alist))

(desktop-save-mode 1)

(require 'kmacro)

;; (global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
;; (global-set-key "\C-w" 'kill-backward-word-or-region)

(global-set-keys
 '(("C-h" . help)
   ("M-h" . mark-paragraph)
   ("C-M-$" . google-define)
   ("C-z" . undo)

   ("C-x C-b" . switch-to-buffer)
   ("C-x x b" . ibuffer)))

(require 'winner)
(winner-mode 1)
(iswitchb-mode 1)

(defvar programming-mode-hooks
  '(c-mode-common-hook
    lisp-mode-hook
    emacs-lisp-mode-hook
    scheme-mode-hook
    perl-mode-hook
    html-mode-hook
    css-mode-hook
    python-mode-hook
    js2-mode-hook))

(add-hooks programming-mode-hooks
           (lambda ()
             ;; (highlight-parentheses-mode 1)
             (highlight-symbol-mode 1)))

(add-hooks '(emacs-lisp-mode-hook python-mode-hook)
           (lambda () (eldoc-mode 1)))

(require 'lang-scripts)
(require 'smooth-scrolling)
(blink-cursor-mode -1)
(require 'lang-mail-rc)

;; (defun pykk-init ()
;;   (interactive)
;;   (insert-file-contents "~/proj/pykk-interactive.py")
;;   (end-of-buffer)
;;   (comint-send-input))

;; (add-hook 'inferior-python-mode-hook
;;           (lambda () (local-set-key "\C-cp" 'pykk-init)))

(defun end-of-linep () (looking-at "$"))

(progn
  (require 'winner)
  (winner-mode 1))

(defmacro define-buffer-visitor (visitor-name buffer-name command)
  "http://jfm3-repl.blogspot.com/2009/02/fast-emacs-buffer-flipping.html"
   `(defun ,visitor-name ()
      (interactive)
      (if (get-buffer ,buffer-name)
	  (switch-to-buffer (if (equal ,buffer-name (buffer-name))
				nil
			      ,buffer-name))
	(call-interactively ,command))))

(require 'irc-rc)
(require 'git-commands)
(require 'nav)
(require 'hide-region)
(require 'moz-rc)
(column-number-mode t)

(progn
  (require 'htmlize)
  (global-set-keys
   '(("C-c l" . org-store-link)
     ("C-c a" . org-agenda)
     ("C-c ." . org-time-stamp)))
  (add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode 1)
              (set-variable 'comment-start nil 'make-local)))
  (require 'org-collector)
  (set-variables
   '(nav-boring-file-regexps (quote ("\\.py[co]$" "\\.o$" "~$" "\\.bak$" "^\\.[^/]" "^\\./?$" "/\\." "\\.min\\.js$" "\\.elc$")))
   '(org-agenda-files (quote ("c:/Documents and Settings/lmartin/My Documents/Code/sample-yui-dds/docs/spec.txt")))
   '(org-enforce-todo-dependencies t)
   '(org-log-done (quote time))
   ))

(require 'google-define)

(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'asp-rc)
(require 'compile-site-lisp)

(progn
  (require 'color-theme)
  (defun color-theme-langmartin ()
    (interactive)
    (color-theme-bharadwaj)
    (let ((color-theme-is-cumulative t))
      (color-theme-install
       (let ((match '((t (:background "lightgoldenrod2"))))
             (modeline '((t (:background "grey75" :foreground "black")))))
         `(color-theme-langmartin
           ((background-color . "ivory"))
           nil
           (erc-current-nick-face ((t (:bold t :foreground "Turquoise4"))))
           (erc-notice-face ((t (:foreground "grey70"))))
           (erc-timestamp-face ((t (:bold t :foreground "green4"))))
           (eshell-prompt ((t (:bold t :foreground "DarkRed"))))
           (isearch ,match)
           (iswitchb-single-match ((t (:inherit font-lock-function-name-face))))
           (font-lock-comment-face ((t (:foreground "grey50"))))
           (font-lock-function-name-face ((t (:foreground "SlateBlue" :slant normal :weight bold))))
           (match ,match)
           (minibuffer-noticeable-prompt ((t (:inherit match))))
           (modeline ,modeline)
           (modeline-buffer-id ((t (:background "LightSlateGrey" :foreground "black"))))
           (modeline-mousable ,modeline)
           (modeline-mousable-minor-mode ,modeline)
           (mode-line-inactive ((t (:background "grey90" :foreground "grey20"))))
           (org-agenda-restriction-lock ((t (:inherit match))))
           (org-clock-overlay ((t (:inherit match))))
           (org-todo ((t (:inherit font-lock-string-face))))
           )))))
  (color-theme-initialize)
  (color-theme-langmartin))

(global-set-keys
 '(("C-x C-c" . nil)
   ("<f1>" . vi-mode)

   ("M-<f4>" . delete-frame)
   ("<f5>" . eshell)
   ("<f6>" . imenu)
   ("<f7>" . normal-mode)
   ("<f8>" . toggle-truncate-lines)

   ("<f10>" . visual-basic-mode)
   ("<f9>" . css-mode)
   ("<f11>" . js2-mode)
   ("<f12>" . rc-sgml-mode-for-asp)))

(progn
  (scroll-bar-mode -1)

  (defun split-window-horizontally-minsize (&optional other)
    "Wrapper for spit-window-horizontally that gives 80 columns"
    (interactive "P")
    (let* ((extra 2) (size (+ extra 80)))
      (if (< (window-width) (+ size extra window-min-width))
          (split-window-horizontally other)
        (if other
            (split-window-horizontally (- (window-width) (- size extra 1)))
          (split-window-horizontally size)))))
  
  (global-set-key (kbd "C-x 3") 'split-window-horizontally-minsize)
  (setq truncate-partial-width-windows 80))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-keys '(("M-`" . switch-to-last-buffer)))

(progn
  (require 'term-mode-rc)
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "ivory"
        term-default-fg-color "black"))
