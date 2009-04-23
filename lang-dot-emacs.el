(load "site-start")
(rc-lang)

;; things for cocoa emacs 23
(progn
  (require 'mwheel)
  (global-set-key [wheel-down] 'mwheel-scroll)
  (global-set-key [wheel-up] 'mwheel-scroll)
  (global-set-key "\M-`" 'other-frame)
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

;; (progn
;;  (defun color-grey () (color-theme-fischmeister) t)
;;  (defun color-slate () (color-theme-subtle-hacker) t)
;;  (and (require 'color-theme "color-theme" t)
;;       (progn (color-theme-initialize)
;;              ;; (color-slate)
;;              ;; (show-paren-mode nil)
;;              (color-grey)
;;              )))

;; (global-set-key "\C-xl" (lambda () (interactive) (insert "lambda")))
;; (global-set-key "\C-w" 'kill-backward-word-or-region)
(global-set-key "\C-h" 'help)

(iswitchb-mode 1)

(setq ring-bell-function nil)

(defvar programming-mode-hooks
  '(c-mode-common-hook
    lisp-mode-hook
    emacs-lisp-mode-hook
    scheme-mode-hook
    perl-mode-hook
    html-mode-hook
    css-mode-hook
    python-mode-hook))

(add-hooks programming-mode-hooks
           (lambda ()
             ;; (highlight-parentheses-mode 1)
             (highlight-symbol-mode 1)))

(add-hooks '(emacs-lisp-mode-hook python-mode-hook)
           (lambda () (eldoc-mode 1)))

(add-hooks '(javascript-mode-hook)
           (lambda () (local-set-key "\C-m" 'newline)))

(require 'cx-timesheet)
(rc-emacs-lisp-action)

(require 'lang-scripts)

(require 'smooth-scrolling)

(blink-cursor-mode -1)

(global-set-key "\C-z" 'undo)

(require 'lang-mail-rc)

;; (defun pykk-init ()
;;   (interactive)
;;   (insert-file-contents "~/proj/pykk-interactive.py")
;;   (end-of-buffer)
;;   (comint-send-input))

;; (add-hook 'inferior-python-mode-hook
;;           (lambda () (local-set-key "\C-cp" 'pykk-init)))

(defun end-of-linep () (looking-at "$"))

;; (require 'toggle-letter-case)

(require 'less)
;; (add-hook 'find-file-hooks 'auto-less-minor-mode)

(progn
  (require 'winner)
  (winner-mode 1))

(global-set-key "\C-x\C-b" 'ibuffer)

(progn
  (require 'term-mode-rc)
  ;; this seems to be a bug in nightly-build, and matches my theme
  (setq term-default-bg-color "gray80"
        term-default-fg-color "black"))

(defmacro define-buffer-visitor (visitor-name buffer-name command)
  "http://jfm3-repl.blogspot.com/2009/02/fast-emacs-buffer-flipping.html"
   `(defun ,visitor-name ()
      (interactive)
      (if (get-buffer ,buffer-name)
	  (switch-to-buffer (if (equal ,buffer-name (buffer-name))
				nil
			      ,buffer-name))
	(call-interactively ,command))))

(require 'timeclock-rc)

(progn
  (defun irc-bitlbee ()
    (interactive)
    (erc-tls :server "testing.bitlbee.org"
	     :port 6668
	     :nick "langmartin"
	     :full-name "Lang Martin"))

  ;; (defun bitlbee-identify ()
  ;;   (when (and (string= "testing.bitlbee.org" erc-session-server)
  ;; 	       (string= "&bitlbee" (buffer-name)))
  ;;     (erc-message "PRIVMSG" (format "%s identify %s" 
  ;; 				     (erc-default-target) 
  ;; 				     "<password>"))))
  ;; (custom-set-variables
  ;;  '(erc-join-hook (quote (bitlbee-identify))))

  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
  (add-hook 'erc-text-matched-hook 'erc-growl-match)
  (erc-match-mode 1)

  (defun erc-hide-notices () "hide all notices in a very busy channel"
    (interactive)
    (make-local-variable 'erc-echo-notice-always-hook)
    (setq erc-echo-notice-always-hook nil))

  '(setq erc-autojoin-channels-alist
	 '(("freenode.net" "#emacs" "#scheme" "#medium")))
  
  (defun irc-freenode ()
    (interactive)
    (erc :server "irc.freenode.net"
	 :nick "langmartin"
	 :full-name "Lang Martin"
	 :password freenode-password))
  
  (define-buffer-visitor visit-medium "#medium" 'irc)
  (global-set-key (kbd "H-m") 'visit-medium)
  
  (defun irc () (interactive) (irc-freenode)))

(progn
  (defmacro define-shellcmd (name cmd)
    `(defun ,name ()
       (interactive)
       (shell-command ,cmd)))
  (define-shellcmd git-status "git status")
  (define-shellcmd git-pull "git pull -q")
  (defun git-merge (branch)
    (interactive "sBranch: ")
    (shell-command (concat "git merge -q " branch)))
  (defun git-push (extra)
    (interactive "sPush: ")
    (shell-command (concat "git push " extra)))
  (define-shellcmd git-branches "git branch -av")
  (defun git-checkout (branch)
    (interactive "sBranch: ")
    (shell-command (concat "git checkout " branch)))
  (define-key ctl-x-map "g"
    (easy-mmode-define-keymap
     '(("s" . git-status)
       ("p" . git-pull)
       ("m" . git-merge)
       ("b" . git-branches)
       ("c" . git-checkout)))))

(require 'nav)
(require 'hide-region)
(require 'moz-rc)

(column-number-mode t)
