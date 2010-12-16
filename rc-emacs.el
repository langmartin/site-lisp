;; -*- no-byte-compile: t -*-

;; http://osdir.com/ml/help-gnu-emacs-gnu/2010-06/msg00050.html
(setq warning-suppress-types nil)

(require 'hooks)
(load "site-start")
(rc-lang)

(defmacro ifdef (function &rest args)
  `(if (fboundp ',function)
       (,function ,@args)))

(require 'rc-javascript)

(ifdef global-subword-mode)

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

;; (add-hooks programming-mode-hooks
;;            (lambda ()
;;              ;; (highlight-parentheses-mode 1)
;;              (highlight-symbol-mode 1)))

(add-hooks '(emacs-lisp-mode-hook python-mode-hook)
           (lambda () (eldoc-mode 1)))

(require 'script-collection)
(require 'smooth-scrolling)
(blink-cursor-mode -1)
(require 'rc-gnus)

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

(require 'rc-erc)
(require 'git-commands)
(require 'hide-region)
(column-number-mode t)
(require 'rc-org-mode)
(require 'google-define)
(require 'rc-asp)
(require 'compile-site-lisp)

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

(require 'rc-hyper-keymap)

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

(set-variables
 '(paren-sexp-mode nil)
 '(partial-completion-mode t)
 '(pgg-default-user-id "Lang Martin")
 '(rst-mode-lazy nil)
 '(scroll-bar-mode nil)
 '(server-raise-frame nil)
 '(temporary-file-directory "~/.emacs.d/tmp/")
 '(tool-bar-mode nil)
 '(track-eol t)
 '(inhibit-startup-screen t)
 '(line-spacing 1)
 '(global-auto-revert-mode t)

 '(LaTeX-command "pdflatex")
 '(TeX-default-mode (quote plain-tex-mode))
 '(column-number-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")

 '(dired-listing-switches "-alh")
 '(eshell-ls-use-colors nil)
 '(eshell-prompt-function (lambda nil (concat (number-to-string eshell-last-command-status) " " (eshell/pwd) (if (= (user-uid) 0) " # " " $ "))))
 '(eshell-visual-commands (quote ("ssh" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"))))

(defun rc-package-install-packages ()
  "Install all my ELPA packages, for posterity"
  (interactive)
  (mapc (lambda (p)
          (package-install p))
        '(highlight-symbol
          htmlize
          guess-style
          magit
          pick-backup
          w3
          wtf)))

;; (progn
;;   (require 'edit-server)
;;   (edit-server-start))

(defun change-coding-system (system)
  (set-buffer-file-coding-system system)
  (save-buffer))

(defun 2dos ()
  (interactive)
  (change-coding-system 'dos))

(progn
  (require 'uuid)
  (defalias 'uuid 'insert-random-uuid))

(if (require 'emms "emms" t)
    (progn
      (require 'emms-player-mplayer)
      (emms-default-players)
      (global-set-key (kbd "C-c e e") 'emms-show)
      (global-set-key (kbd "C-c e <up>") 'emms-pause)
      (global-set-key (kbd "C-c e <down>") 'emms-pause)
      (global-set-key (kbd "C-c e <left>") 'emms-previous)
      (global-set-key (kbd "C-c e <right>") 'emms-next)))

(require 'rainbow-mode)

(progn
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist)))

;; (require 'rc-slime)

(toggle-text-mode-auto-fill)

;;;; http://emacs-grasshopper.blogspot.com/2010/12/one-key-to-delete-whitespace.html
(progn
  (defvar ph/delete-whitespace-counter 0)

  (defun ph/delete-whitespace (arg)
    (interactive "*p")
    (if (eq last-command 'ph/delete-whitespace)
        (progn
          (incf ph/delete-whitespace-counter)
          (if (= ph/delete-whitespace-counter 1)
              (delete-blank-lines)
            (join-line arg)))
      (setq ph/delete-whitespace-counter 0)
      (just-one-space arg)))

  (global-set-key (kbd "S-SPC") 'ph/delete-whitespace))

(defun rc-main-emacs-session ()
  "A bunch of settings that should only be applied to my primary,
long-running emacs session. It's useful to have these optional so
that I can avoid a bunch of warnings if I open a new emacs to run
something that needs a second thread."
  (interactive)
  (rc-maybe-session)
  (require 'edit-server)
  (edit-server-start)
  (require 'rc-look-and-feel)
  (server-mode 1)
  (global-set-keys '(("C-x C-c" . nil))))

(provide 'rc-emacs)