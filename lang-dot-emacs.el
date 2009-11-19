;; -*- no-byte-compile: t -*-

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

;; (desktop-save-mode 1)

(require 'kmacro)

;; (global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
;; (global-set-key "\C-w" 'kill-backward-word-or-region)

(global-set-keys
 '(("C-M-$" . google-define)
   ("C-z" . undo)
   ("C-x C-b" . switch-to-buffer)
   ("C-x x b" . ibuffer)))

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
(require 'gnus-rc)

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
  (define-key winner-mode-map (kbd "C-c <C-right>") 'winner-undo)
  (define-key winner-mode-map (kbd "C-c <C-left>") 'winner-undo)
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
(require 'org-mode-rc)
(require 'google-define)
(require 'asp-rc)
(require 'compile-site-lisp)

(progn
  (eval-when-compile (require 'color-theme))
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
           (trailing-whitespace ((((class color) (background light)) (:background "ivory3"))))
           )))))
  (color-theme-initialize)
  (color-theme-langmartin))

(defun rc-show-paren-expression ()
  (interactive)
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match "grey95")
  (set-face-background 'show-paren-mismatch "MediumPurple2"))

(defun rc-show-paren-parens ()
  (interactive)
  (setq show-paren-style 'parenthesis)
  (set-face-background 'show-paren-match "grey80")
  (set-face-background 'show-paren-mismatch "purple")
  (set-face-foreground 'show-paren-mismatch "white"))

(rc-show-paren-expression)

(global-set-keys
 '(
   ("C-x C-j" . execute-extended-command)
   ("C-c C-j" . execute-extended-command)

   ("<f1>" . vi-mode)

   ("M-<f4>" . delete-frame)
   ("<f5>" . eshell)
   ("<f6>" . imenu)
   ("<f7>" . revert-buffer)
   ("<f8>" . toggle-truncate-lines)

   ;;; Replaced by html-script, which binds f12
   ;; ("<f10>" . visual-basic-mode)
   ;; ("<f9>" . css-mode)
   ;; ("<f11>" . js2-mode)
   ;; ("<f12>" . rc-sgml-mode-for-asp)
   ))

(require 'html-script)

(progn
  (require 'chop)
  (global-set-keys
   '(("<C-up>" . chop-move-up)
     ("<C-down>" . chop-move-down))))

(progn
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  (defun split-window-horizontally-minsize (&optional other)
    "Wrapper for spit-window-horizontally that gives 80 columns"
    (interactive "P")
    (let* ((extra (if scroll-bar-mode 5 2)) (size (+ extra 80)))
      (if (< (window-width) (+ size extra window-min-width))
          (split-window-horizontally other)
        (if other
            (split-window-horizontally (- (window-width) (- size extra 1)))
          (split-window-horizontally size)))))

  (global-set-key (kbd "C-x 3") 'split-window-horizontally)
  (setq truncate-partial-width-windows 80))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-keys '(("M-`" . switch-to-last-buffer)))

;; (mapcar (lambda (frame)
;;           (other-buffer nil nil frame))
;;         (frame-list))

(progn
  (require 'term-mode-rc)
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "ivory"
        term-default-fg-color "black"))

(defun add-to-alist (list-sym element &optional append)
  (add-to-list list-sym element append
               (lambda (a b)
                 (eq (car a) (car b)))))

(defun update-alist (alist-symbol element)
  (set alist-symbol
       (cons element
             (filter (lambda (x)
                       (if (eq (car x) (car element))
                           nil
                         x))
                     (symbol-value alist-symbol)))))

(progn
  (fringe-mode '(1 . 1))
  (update-alist 'default-frame-alist '(width . 162))
  (update-alist 'default-frame-alist '(height . 76)))

(progn
  (require 'goto-last-change)
  (global-set-key "\C-x\C-\\" 'goto-last-change))

;;;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))
