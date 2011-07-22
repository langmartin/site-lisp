;;; vc-darcs.el --- VC backend for the darcs revision control system

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: tools
;; Created: June 2004
;; $Revision: 1.73 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify2
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version , or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See <URL:http://www.darcs.net/> concerning darcs.

;; Load this library to register darcs support in VC.
;; It should work with darcs 1 and darcs 2, in Emacs 21, 22, and 23,
;; which accounts for some messiness.

;; There's another implementation like this
; <URL:http://www.pps.jussieu.fr/~jch/software/repos/vc-darcs>, but
;; it doesn't have a copyright assignment for inclusion in Emacs.  See
;; <URL:http://chneukirchen.org/repos/darcsum> for a more PCL-CVS-like
;; interface more appropriate for a patch-based system.

;; This doesn't deal with revisions in many commands, since darcs is
;; patch-based.  The `diff' and `checkout' handlers (currently) do
;; deal with revision specs.  See the doc of `vc-darcs-rev-to-flags'
;; about revisions; normally we use patch name matches instead of
;; revision names.  However, the VC framework can't deal well with
;; this -- at least avoid slashes in anything specified as a revision
;; name.

;; There are plenty of loose ends: see the Fixmes below, some of which
;; may be obsolete now.  VC and log-view need other changes to work
;; more sanely with this darcs stuff -- they still tend to assume
;; RCS/CVS-like features.

;; The package protect-files.el can be used to avoid backup and
;; autosave files in _darcs (see darcs doc).

;;; Code:

;; The following (Emacs 22) VC operations aren't implemented and
;; probably can't or shouldn't be:

;; latest-on-branch-p, init-version [not relevant], receive-file,
;; make-version-backups-p, check-headers [doesn't expand headers],
;; clear-headers [likewise], find-file-hook, find-file-not-found-hook,
;; cancel-version [darcs obliterate isn't file-based], merge-news
;; [probably not meant to do something like pull], steal-lock [no
;; locking], state-heuristic [maybe should be], merge,
;; comment-history, update-changelog, assign-name, retrieve-snapshot.

;; Some of these probably could/should be implemented for Emacs 23,
;; but I don't understand what some of them are supposed to do, and
;; that version of VC still doesn't seem to deal properly with
;; patch-oriented VCS.  Note also the unfortunate need for advising
;; functions below.

;; dir-printer, dir-menu, status-fileinfo-extra, prettify-state-info,
;; rollback [check if it's really file-based, and if that's recent],
;; modify-change-comment, mark-resolved, revision-completion-table,
;; retrieve-tag extra-dir-menu, extra-status-menu, latest-on-branch-p


;; Fixmes (more in the code):
;;
;; * Figure out whether to attempt to do anything with coding
;;   conversion of darcs i/o where relevant.  darcs is currently
;;   unreliable with non-ASCII.
;;
;; * Merge might be implemented as applying patch, but merging to a
;;   single file presumably requires pulling to a scratch branch.
;;
;; * Should retrieve-snapshot do darcs get?
;;
;; * Implement update-changelog, but that requires convention for how
;;   to deal with darcs comments.

(eval-when-compile
  (require 'vc))			; for vc-exec-after

(defgroup vc-darcs nil
  "VC darcs backend."
;;   :version "22"
  :group 'vc)

(defcustom vc-darcs-program "darcs"
  "*Name of the darcs command (excluding any arguments)."
  :group 'vc-darcs
  :type 'string)

;; Fixme: there's probably no call for this as a user option (but see
;; vc-darcs-checkin).
(defcustom vc-darcs-program-args nil
  "*List of global arguments to pass to `vc-darcs-program'."
  :group 'vc-darcs
  :type '(repeat string))

(defcustom vc-darcs-author (concat user-full-name " <" user-mail-address ">")
  "Fallback value for author email address.
See function `vc-darcs-author'."
  :group 'vc-darcs
  :type 'string)

(defcustom vc-darcs-diff-switches nil
  "*String/list of strings specifying extra switches for darcs diff under VC."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :group 'vc-darcs)

(defconst vc-darcs-23 (> emacs-major-version 22)
  "Non-nil when dealing with the Emacs 23 version of VC.")

(defun vc-darcs-command (command buffer okstatus file &rest args)
  "Wrapper round `vc-do-command' using `vc-darcs-program' as COMMAND."
  ;; Make sure we avoid ptys to discourage any termcap escapes.
  (let ((process-connection-type nil))
    (apply 'vc-do-command
	   ;; Emacs pre-23 doesn't accept a null buffer, sigh.
	   (or buffer "*vc*")
	   okstatus vc-darcs-program
	   ;; This doesn't appear actually to be necessary:
	   ;; (and file (file-name-nondirectory file))
	   file
	   command (append vc-darcs-program-args args '("--")))))

;; If we're only interested in messages like "No changes!", we only
;; need to set LC_MESSAGES, and we might need finer control of this.
;; Actually darcs isn't localized yet, so this is future-proof.
(defmacro vc-darcs-with-c-locale (&rest body)
  "Run BODY with LC_ALL=C in the process environment.
This ensures that messages to be matched come out as expected."
  `(let ((process-environment (cons "LC_ALL=C" process-environment)))
     ,@body))

;; Fixme: Is it worth trying to use `show repo' to get `Root:'?
(defun vc-darcs-_darcs-dir (file)
  "Return the _darcs directory in the hierarchy above FILE.
Return nil if there isn't one."
  (or (vc-file-getprop file 'darcs-dir)
      (if (fboundp 'vc-find-root)
	  (let ((dir (vc-find-root file "_darcs")))
	    (when dir
	      (setq dir (expand-file-name "_darcs" dir))
	      (vc-file-setprop file 'darcs-dir dir)
	      dir))
	(let ((dir (if (file-directory-p (setq file (expand-file-name file)))
		       file
		     (file-name-directory file)))
	      _darcs)
	  (vc-file-setprop
	   file 'darcs-dir
	   (catch 'found
	     (while t
	       (setq _darcs (expand-file-name "_darcs" dir))
	       (if (file-directory-p _darcs)
		   (throw 'found (file-name-as-directory _darcs)))
	       (if (equal "" (file-name-nondirectory
			      (directory-file-name dir)))
		   (throw 'found nil)
		 (setq dir
		       (file-name-directory (directory-file-name dir)))))))))))

(defun vc-darcs-root (file)
  "Return repo directory containing FILE or nil if FILE isn't registered."
  (let ((_darcs (vc-darcs-_darcs-dir file)))
	(if _darcs (file-name-directory (directory-file-name _darcs)))))

;; Only works for darcs 1-format repos.
(defun vc-darcs-current (file)
  "Find the current version of FILE in _darcs/current or  _darcs/pristine.
Return the file name if it exists."
  (let* ((top (vc-darcs-root file))
	 ;; Don't invoke vc-darcs-_darcs-dir twice.
	 (_darcs (expand-file-name "_darcs" top))
	 (current (if _darcs
		      (expand-file-name "current" _darcs))) ; old-style
	 (current (if (and current (file-exists-p current))
		      (file-name-as-directory current)
		    (file-name-as-directory
		     (expand-file-name "pristine" _darcs))))
	 found)
    (when (and (string-match (concat "\\`" (regexp-quote top) "\\(.*\\)\\'")
			     file)
	       (setq found (expand-file-name (match-string 1 file) current))
	       (file-exists-p found))
      found)))

(defvar vc-darcs-features nil
  "Feature alist for `vc-darcs-program'.
Used by `vc-darcs-feature'.")

;; Fixme: check when all these actually came in -- can we just test
;; for one, showing darcs 2, for instance?
(defun vc-darcs-feature (feature)
  "Return non-nil if `vc-darcs-program' supports FEATURE (a symbol).
The features and corresponding supported arguments are:
`show-files': show files
`show-contents': show contents
`show-repo': show repo
`changes-only': changes --only-to-files
`2': darcs version 2"
  (let ((elt (assq feature vc-darcs-features)))
    (if elt				; value cached
	(cdr elt)
      (push				; else update cache
       (cons feature
	     (with-temp-buffer
	       (cond ((eq feature 'show-files)
		      (vc-darcs-command "show" t nil nil "files" "--help")
		      (re-search-backward "\\`Usage: darcs show files" nil t))
		     ((eq feature 'show-contents)
		      (vc-darcs-command "show" t nil nil "contents" "--help")
		      (re-search-backward "\\`Usage: darcs show contents"
					  nil t))
		     ((eq feature 'show-repo)
		      (vc-darcs-command "show" t nil nil "repo" "--help")
		      (re-search-backward "\\`Usage: darcs show repo" nil t))
		     ((eq feature 'changes-only)
		      (vc-darcs-command "changes" t nil nil "--help")
		      (re-search-backward "--only-to-files" nil t))
		     ((equal feature 2)
		      ;; Can't just do this, as darcs fails with args
		      ;; `--version -- .':
		      ;; (vc-darcs-command "--version" t nil nil)
		      ;; This is good enough.
		      (vc-darcs-feature 'show-files))
		     ;; Fixme:  was --quiet always present?
		     ((equal feature 'quiet)
		      (vc-darcs-command "diff" t nil nil "--help")
		      (re-search-backward "-q +--quiet" nil t)))))
	    vc-darcs-features)
      ;; Check updated cache:
      (vc-darcs-feature feature))))

;; Fixme:  This doesn't indicate an unrecorded move of FILE.  Should
;; we worry?
(defun vc-darcs-registered (file)
  "Return non-nil if FILE is registered with darcs."
  (let* ((top (vc-darcs-root file))
	 (case-fold-search nil))
    ;; Checking `top' first assumes that it's cheap compared with
    ;; running darcs.
    (and top
	 ;; Avoid deadlock with darcs invoking emacsclient.  When it
	 ;; does this while doing a record, for instance, the
	 ;; repository is locked, and that prevents `darcs query'
	 ;; running in the repository.  The trouble is that the
	 ;; temporary file that darcs uses for editing is
	 ;; `.darcs-record' (or `.darcs-temp-mail',
	 ;; `darcs-temp-mail-N') _in the repository_, so when the
	 ;; server process tries to find it, it just hangs.
	 ;; <URL:http://bugs.darcs.net/issue315>.  This shouldn't be
	 ;; an issue as of darcs 1.0.9rc2.
	 (not (string-match "^\\.darcs-\\|^darcs-temp-mail-"
			    (file-name-nondirectory file)))
	 (or (vc-darcs-current file)
	     ;; Fixme: save needed if vc-darcs-command fails -- check
	     ;; why.
	     (save-window-excursion
	       ;; Don't error out and fail to display found file if
	       ;; darcs isn't available.
	       (condition-case ()
		   (with-temp-buffer
		     (cd (file-name-directory file))
		     ;; We can't DTRT for boring files with darcs 1.
		     (if (vc-darcs-feature 2)
			 (progn ;vc-darcs-with-c-locale
			   ;; In earlier version 2s, we get status 1
			   ;; with no changes.
			   (vc-darcs-command "whatsnew" t 1 file
					     "-l" "--summary")
			   (not
			    (or (let (case-fold-search)	; addable
				  (goto-char (point-max))
				  (forward-line -1)
				  (looking-at "a \\."))
				(progn	; boring or really doesn't exist
				  (goto-char 1)
				  (re-search-forward
				   " not in repository!$\\| does not exist!$"
				   (line-end-position) t)))))
		       ;; whatsnew doesn't distinguish boring files
		       ;; from unchanged registered files with darcs 1.
		       (vc-darcs-command "query" t 0 nil "manifest")
		       ;; Deal with (decimal) escapes, e.g. space in
		       ;; file names is `\32\'.
		       (goto-char 1)
		       (while (re-search-forward "\\\\\\([0-9]+\\)\\\\" nil t)
			 (replace-match
			  (string (string-to-number (match-string 1)))))
		       (goto-char 1)
		       ;; Canonicalize the names in the list before
		       ;; comparing them.
		       (catch 'result
			 (while t
			   (if (equal (expand-file-name file)
				      (expand-file-name
				       (buffer-substring
					(line-beginning-position)
					(line-end-position))
				       top))
			       (throw 'result t)
			     (if (/= 0 (forward-line))
				 (throw 'result nil)))))))
		 (error nil)))))))

(defcustom vc-darcs-state-check 'darcs
  "How to check up-to-date state of darcs-controlled file.
Possible values in order of speed are:
`file-times': compare modification times of current and working files;
`cmp': use cmp(1) to compare current and working files;
`darcs'': up-to-date if `darcs whatsnew' says `no changes' -- usually too
slow."
  :group 'vc-darcs
  :type '(choice (const :tag "Compare modification times" file-times)
		 (const :tag "Use cmp(1)" cmp)
		 (const :tag "Use `darcs whatsnew'" darcs)))

(defun vc-darcs-state (file)
  "Check state of FILE (edited or up-to-date).  See `vc-darcs-state-check'."
  (let ((case-fold-search nil)
	(root (vc-darcs-root file)))
    (if (eq 'darcs vc-darcs-state-check) ; proper, slow (?)
	(with-temp-buffer
	  (vc-darcs-with-c-locale (vc-darcs-command "whatsnew" t 1 file
						    "--summary" "-l"))
	  ;; Without -l it says no changes for unregistered file with
	  ;; darcs 1.  (--summary is currently redundant with -l, but
	  ;; seems more robust.)  Also with darcs 1 we just get `No
	  ;; changes' with a boring file, but too bad.  In 2.2, a
	  ;; boring file gets `not in repository', but in 2.3 it gets
	  ;; `does not exist'.
	  (goto-char (point-max))
	  (beginning-of-line 0)
	  (if (not vc-darcs-23)
	      (if (looking-at "No changes!$")
		  'up-to-date
		'edited)
	    (cond
	     ((looking-at "[A-Za-z]!") 'conflict)
	     ((looking-at "a ") 'unregistered)
	     ((looking-at "A ") 'added)
	     ((looking-at "R ") 'removed)
	     ((looking-at "No changes!$")
	      (goto-char 1)
	      (if (looking-at "WARNING: File '\\(.*\\)' \\(?:
\\(does not exist\\)\\|\\(not in repository\\)\\)!")
		  'ignored
		'up-to-date))
	     ((looking-at " \\(.*\\) -> \\(\\..*\\)$")
	      (if (equal file (save-match-data
				(expand-file-name (match-string 1) root)))
		  'removed
		(if (equal file (expand-file-name (match-string 2) root))
		    'added)))
	     (t 'edited))))
      ;; This doesn't deal with renames and isn't really appropriate
      ;; for Emacs 23.
      (let ((current (vc-darcs-current file)))
	(if current
	    (if (and (eq 'cmp vc-darcs-state-check) ; actually always correct?
		     (executable-find "cmp"))
		(if (equal ""
			   (shell-command-to-string
			    (format "cmp %s %s" (shell-quote-argument file)
				    (shell-quote-argument current))))
		    'up-to-date
		  'edited)
	      ;; Else conservative guess with mod times.
	      (if (file-newer-than-file-p file current)
		  'edited
		'up-to-date))
	  ;; darcs 2, for instance
	  (let ((vc-darcs-state-check 'darcs))
	    (vc-darcs-state file)))))))

(defconst vc-darcs-working-name "<working>"
  "Pseudo-revision used for the workfile version.")

(defun vc-darcs-workfile-version (file)
  "Return the supposed workfile version of FILE.
Actually just returns a constant bogus (we hope) value."
  vc-darcs-working-name)

;; Avoids warning in Emacs 23
(defun vc-darcs-working-revision (file)
  "Return the supposed workfile version of FILE.
Actually just returns a constant bogus (we hope) value, or nil if FILE
is unregistered."
  (when (vc-registered file) vc-darcs-working-name))

(defun vc-darcs-checkout-model (file)
  'implicit)

(defun vc-darcs-workfile-unchanged-p (file)
  "Return non-nil if FILE has not changed since the last darcs checkout."
  (eq 'up-to-date (vc-darcs-state file)))

(defun vc-darcs-mode-line-string (file)
  "Return mode line string for darcs-controlled FILE.
This is `darcs/' followed by `*' if the file is edited and `-' otherwise."
  (format "darcs:%c"
	  (let ((state (vc-state file)))
	    (cond ((eq state 'added) ?a)
		  ((eq state 'removed) ?r))
	  (if (eq 'edited (vc-state file)) ?* ?-))))

(defun vc-darcs-register (fileset &optional rev comment)
  "Register FILESET under darcs.
FILESET is a file or list of files.
Signal an error unless REV is nil or `vc-default-init-version'.
COMMENT is ignored.
If necessary, parent directories are also registered or the current
directory is initialized."
  (if (and rev (if (boundp 'vc-default-init-version)
		   (not (equal rev vc-default-init-version)))
	   (if (boundp 'vc-default-init-revision)
	       (not (equal rev vc-default-init-revision))))
      (error "Can't register explicit version with darcs"))
  (dolist (file (if (consp fileset) fileset (list fileset)))
    (if (vc-darcs-responsible-p file)
	(unless (vc-darcs-could-register file) ; registered or is boring
	  (error "Can't register %s with darcs" file))
      ;; Initialize directory and re-try.
      (vc-darcs-create-repo)
      (unless (vc-darcs-could-register file)
	(error "Initialized directory, but can't register %s with darcs"
	       file))))
  (vc-darcs-command "add" nil 0 fileset))

;; Could run `darcs changes' in the directory and see if it succeeds,
;; but that could be expensive with many changes.
(defun vc-darcs-responsible-p (file)
  "Return non-nil if FILE is (potentially) controlled by darcs.
The criterion is that there is a `_darcs' directory in the same
or a superior directory."
  (and (vc-darcs-_darcs-dir file) t))

(defun vc-darcs-could-register (file)
  "Return non-nil if FILE could be registered under darcs."
  (when (vc-darcs-responsible-p file)	; shortcut
    (condition-case ()
	(with-temp-buffer
	  (vc-darcs-command "add" t 1 file "--dry-run")
	  t)
      (error))))

(defun vc-darcs-unregister (file)
  "Unregister FILE from darcs."
  (vc-darcs-command "remove" nil 0 file))

(defun vc-darcs-author (file)
  "Return author information for FILE.
Checks the prefs/author and ~/.darcs/author files, the DARCS_EMAIL
and EMAIL environment variables, and finally `vc-darcs-author'."
  (or (let ((author (expand-file-name
		     "author"
		     (file-name-as-directory
		      (expand-file-name "prefs" (vc-darcs-_darcs-dir file))))))
	(unless (file-exists-p author)
	  (setq author "~/.darcs/author"))
	(and (file-exists-p author)
	     (with-temp-buffer
	       (insert-file-contents author)
	       (if (looking-at "[ \t]*\\([^ \t]+.*\\)$")
		   (match-string 1)))))
      (getenv "DARCS_EMAIL")
      (getenv "EMAIL")
      vc-darcs-author))

;; This seems to be the right default for VC.
(defcustom vc-darcs-elide-tests t
  "Non-nil means don't run a test script when checking in changes with darcs,
i.e. use the darcs option --no-test.  You probably don't want to run
time-consuming tests when using VC."
  :group 'vc-darcs
  :type 'boolean)

(defun vc-darcs-checkin (file rev comment)
  "Check FILE in to darcs with log message COMMENT.
REV non-nil gets an error.
The first line of COMMENT is used as the patch name.  If it is
blank, the time string is used for the name.  Subsequent lines
are used as the darcs `long comment'.
By default this won't run any test script; see `vc-darcs-elide-tests'."
  (if rev (error "Can't check in a specific version with darcs"))
  (if (or (equal comment "*** empty log message ***")
	  (= 0 (length comment)))
      (error "Can't have empty log message"))
  (let* ((time (format-time-string "%Y%m%d%H%M%S"))
	 ;; Match comment as groups of first line (less trailing
	 ;; newline) and the rest.
	 (match (string-match "\\`\\([^\n]+\\)\\(\n\\(?:.\\|\n\\)*\\)?\\'"
			      comment))
	 (log (or (and match (match-string 2 comment))
		  ""))
	 (patch-name (if match
			 (match-string 1 comment)
		       time)))
    (let ((vc-darcs-program-args vc-darcs-program-args))
      (if vc-darcs-elide-tests (push "--no-test" vc-darcs-program-args))
      ;; Fixme: use --logfile?
      (vc-darcs-command "record" nil 'async file "-a" "--pipe"))
    (with-current-buffer "*vc*"
      (process-send-string nil (concat time "\n" (vc-darcs-author
						  (if (consp file)
						      (car file)
						    file))
				       "\n" patch-name))
      (process-send-string nil log)	; has leading newline
      (unless (equal ?\n (aref log (1- (length log))))
	(process-send-string nil "\n"))
      (process-send-eof))))

(defcustom vc-darcs-patch-program (or (bound-and-true-p ediff-patch-program)
				      "patch")
  "*Name of the program that applies patches."
  :group 'vc-darcs
  :type 'string)

;; Fixme:  This fails for the initial patch since the diff for it is
;; empty -- seems to be a darcs bug.  Actually this isn't the case if
;; you record the two patches comprising the initial checkin
;; separately, i.e. the addfile and the subsequent diff for the file
;; contents.

;; Fixme:  The VC level needs to be able to sanitize REV and mangle it
;; (to a hash?), specially in the case of a rev like `date ... &
;; author ...'.  See the advice below in the meantime.
(defun vc-darcs-checkout (file &optional editable rev destfile)
  "Checkout FILE from darcs to DESTFILE.
EDITABLE is ignored.  \"Revision\" REV matches the most recent patch
name for FILE."
  (unless destfile (setq destfile file))
  (let ((mode (file-modes file)))
    (if (vc-darcs-feature 'show-contents)
	(with-temp-file destfile
	  (if (and rev (not (equal rev (vc-darcs-workfile-version file))))
	      (apply #'vc-darcs-command "show" t 0 file "contents"
		      (vc-darcs-rev-to-flags rev))
	    (vc-darcs-command "show" t 0 file "contents")))
    (if (or (not rev) (string= rev vc-darcs-working-name))
	(copy-file (vc-darcs-current file) destfile t t)
      (let ((tmp (make-temp-file "darcs")))
	(condition-case data
	    (with-temp-buffer
	      (copy-file (vc-darcs-current file) tmp t)
	      (vc-darcs-diff file rev nil (current-buffer))
	      (unless (equal 0
			     (call-process-region 1 (point-max)
						  vc-darcs-patch-program
						  t t nil
						  "-t" "-o" destfile tmp))
		(signal 'error (list (buffer-string)))))
	  (error (error "Patching failed: %S" data)))
	(delete-file tmp))))
    ;; We inherit read-only from the repository version -- fix
    ;; permissions to be those of FILE.
    (set-file-modes destfile mode)))

;; Fixme: Just copy current file?
(defun vc-darcs-revert (file &optional contents-done)
  (unless contents-done
    (vc-darcs-command "revert" nil 'async file "-a" file)))

(defcustom vc-darcs-print-log-summary nil
  "*Non-nil means \\[vc-print-log] for darcs should print summary information.
This shows the extent of changes to the (single) file."
  :type 'boolean
  :group 'vc-darcs)

(eval-when-compile
  (defvar log-view-message-re)
  (defvar log-view-file-re)
  (defvar log-view-font-lock-keywords)
  (defvar log-view-current-tag-function))

;; Grim hack to account for lack of an extension mechanism for
;; log-view.  That's taken care of for Emacs 23 (see
;; vc-darcs-log-view-mode).
(defun vc-darcs-log-view-hook (&optional file)
  "To be added to `log-view-mode-hook' to set variables for darcs o/p.
Removes itself after running."
  (remove-hook 'log-view-mode-hook 'vc-darcs-log-view-hook)
  (vc-darcs-log-view-mode))

(defun vc-darcs-get-changes (buffer status file &rest args)
  "Run `darcs changes' in BUFFER for FILE and `vc-darcs-command' STATUS.
Use also the extra ARGS, the last of which may be a list, to run
something like
 (apply #'vc-darcs-command \"changes\" BUFFER STATUS FILE ARGS)"
  (if (and (listp (last args)) (> (length args) 1))
      (setq args (append (butlast args 1) (car (last args)))))
  ;; Force UTC so that we can rely on `match date' working with the
  ;; printed timestamps, e.g. in `log-view-current-tag-function'.
  (let ((process-environment (cons "TZ=UTC" process-environment)))
    (if (and file (vc-darcs-feature 'changes-only))
		  (push "--only-to-files" args))
    (apply #'vc-darcs-command "changes" buffer status file args)))

(defun vc-darcs-print-log (files &optional buffer shortlog start-rev limit)
  "Get darcs change log for FILE into specified BUFFER.
See also `vc-darcs-print-log-summary'."
  (if vc-darcs-print-log-summary
      (vc-darcs-get-changes buffer 'async files "--summary")
    (vc-darcs-get-changes buffer 'async files "--no-summary"))
  (unless (fboundp 'vc-dir)		; Emacs 23?
    (unless (fboundp 'vc-log-view-hook)	; proposed extension
      (add-hook 'log-view-mode-hook 'vc-darcs-log-view-hook))))

(eval-after-load "log-view"
  '(unless (fboundp 'vc-default-log-view-current-tag-function)
     (defvar log-view-current-tag-function nil)
     (defadvice log-view-current-tag (around darcs activate)
       "Add `log-view-current-tag-function' handler."
       (if log-view-current-tag-function
	   (setq ad-return-value
		 (funcall log-view-current-tag-function (ad-get-arg 1)))
	 ad-do-it))))

(defun vc-darcs-log-view-current-tag (&optional where)
  "Handler function for `log-view-current-tag' for darcs."
  (save-excursion
    (when where (goto-char where))
    (forward-line 1)
    (let ((pt (point)))
      (when (re-search-backward log-view-message-re nil t)
	(let* ((date (match-string-no-properties 1))
	       (author (buffer-substring-no-properties (match-end 0)
						       (line-end-position)))
	       (log (buffer-substring-no-properties
		     (+ (line-beginning-position 2) 4)
		     (line-end-position 2)))
	       tag)
	  (if (string-match "\\`tagged " log)
	      (setq tag (substring log 7)))
	  (format "exact %S && date %S && author %S"
		  (or tag log) date author))))))

(defun vc-darcs-posix-to-re (re)
  "Attempt to convert a POSIX-style regexp RE to an Emacs one."
  ;; Fixme: is the editing complete and correct?  (It could at least go
  ;; wrong in bracket expressions):
  ;; \{\} -> {}, \(\) -> (), () -> \(\), | -> \|
  (replace-regexp-in-string "\\\\[(){}]\\|[(){}|]"
			    (lambda (match)
			      (if (= 1 (length match))
				  (concat "\\\\" match)
				(substring match 1)))
			    re))

;; Fixme:  Fix for the current extended rev convention -- search for
;; all components of date && author && exact-match.
(defun vc-darcs-show-log-entry (version)
  "Find entry for patch name VERSION in darcs change log buffer."
  (goto-char (point-min))
  (let (case-fold-search)
    (if (re-search-forward
	 (concat "^ *\\* .*(?:" (vc-darcs-posix-to-re version) ")") nil t)
	(beginning-of-line)
      (goto-char (point-min)))))

(defun vc-darcs-wash-log (file)
  (flush-lines "^\\S-")
  (delete-char 1)			; blank line
  (while (re-search-forward "^  \\* " nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "^ +" nil t)
    (replace-match ""))) ; avoid RCS-style default version

(defun vc-darcs-logentry-check ()
  "Check the form of the log entry for darcs.
The first line must be a patch name, which is forced to be at
least two non-whitespace characters."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (unless (looking-at "^[ \t]*\\(?:[^ \t\n] *\\)\\{2\\}")
	(error "Need non-trivial patch name on first line of log")))))

(autoload 'vc-diff-switches-list "vc" nil nil t)

(defun vc-darcs-diff (file &optional rev1 rev2 buffer)
  "VC darcs backend for diff.
Special-case: if REV1 or REV2 is \"-\", show changes from single
patch named by the other rev."
  (let ((working (vc-darcs-workfile-version file)))
    (if (and (equal rev1 working)
	     (equal rev2 working)
	     (atom file)
	     (vc-darcs-current file))
	;; Use plain diff as optimization when diffing against
	;; working.  Fixme:  Is this (now) worth it?
	(apply #'vc-do-command (or buffer "*vc-diff*") nil diff-command file
	       (append (vc-diff-switches-list darcs)
		       (list (vc-darcs-current file))))
      (apply #'vc-darcs-command "diff" (or buffer "*vc-diff*") 1 file
	     (if (vc-darcs-feature 'quiet) "--quiet")
	     "--diff-opts" (mapconcat 'identity
				      (vc-diff-switches-list darcs)
				      " ")
	     (if (or (equal "-" rev1)
		     (equal "-" rev2)
		     (equal rev1 rev2))
		 ;; Special case that we can't expose to the VC user
		 ;; interface:  if either revision is "-", show
		 ;; changes of single patch.
		 (progn
		   (if (or (equal rev1 vc-darcs-working-name)
			   (equal rev2 vc-darcs-working-name))
		       (error "Patch name not specified"))
		 (vc-darcs-rev-to-flags (if (equal "-" rev2) rev1 rev2)))
	       (append (if (and rev1 (not (equal rev1 working)))
			   (vc-darcs-rev-to-flags rev1 'from))
		       (if (and rev2 (not (equal rev2 working)))
			   (vc-darcs-rev-to-flags rev2 'to))))))))

(defun vc-darcs-rev-to-flags (rev &optional from/to)
  "Derive an argument list specifying patch REV.

If FROM/TO is `from', this is the start of a range; if it is `to',
this is the end of a range; otherwise it is a single patch.

The start of REV determines whether it is treated as a tag, a match
specification, or a patch name match.  If the prefix of REV is `tag ',
the suffix is taken as a tag name.  If its prefix is `(', `exact ',
`name ', `hash ', `date ', or `author ', REV is taken as a
`--match'-style specification.  Otherwise REV is taken as a
`--patch'-style specification."
  (setq from/to (cond ((eq 'from from/to)
		       "from-")
		      ((eq 'to from/to)
		       "to-")))
  (when rev
    (let (case-fold-search)
      (list
       (cond
	((string-match "^\\(?:\(\\|exact \\|name \\|hash \\|date \\|author \\)"
		       rev)
	 (concat "--" from/to "match"))
	((string-match "^tag " rev)
	 (setq rev (substring rev 4))
	 (concat "--" from/to "tag"))
	(t
	 (concat "--" from/to "patch")))
       rev))))

;; This is typically too slow.
;; At least with GNU utils we could do this iff we're at the top level,
;; else adjust relative to top level.
;;   diff -u -r _darcs/current .|grep -v ^Only

(defun vc-darcs-diff-tree (backend dir rev1 rev2)
  "List differences for all registered files at and below DIR.
Revisions REV1 and REV2 are ignored."
  (let ((default-directory dir))
    (vc-darcs-diff "." rev1 rev2)))

(autoload 'vc-snapshot-precondition "vc")

(defun vc-darcs-create-snapshot (dir name branchp)
  "Create snapshot for directory DIR.
Actually tags it with tag NAME.  Gets an error if BRANCHP is non-nil."
  (when branchp
    (error "VC darcs backend does not support module branches"))
  (let ((result (vc-snapshot-precondition dir)))
    (if (stringp result)
	(error "File %s is not up-to-date" result)
      (vc-darcs-command "tag" nil 'async nil "--pipe")
      (with-current-buffer "*vc*"
	(process-send-string nil (format "%s\n%s\n%s\n"
					 (format-time-string  "%Y%m%d%H%M%S")
					 (vc-darcs-author dir) name))
	(process-send-eof)))))

(defun vc-darcs-dir-state (dir)
  (with-temp-buffer
    (cd dir)				; fixme:  necessary?
    ;; Could use --ignore-times.
    ;; NB. this also produces output for any higher-level directories in
    ;; this darcs tree; I don't think that matters.
    ;; Status 2 happens if we end up running this in a non-darcs directory.
    (vc-darcs-command "whatsnew" t 2 nil "--summary" "-l")
    (goto-char (point-min))
    (let (conflict file type case-fold-search)
      ;; The possibilities for the leading tag are: `A'dded,
      ;; `M'odified, `R'emoved.  A following `!' indicates a conflict.
      ;; Can also have `a' for unregistered file.
      ;; Fixme:  Sort out renames.
      (while (re-search-forward "^\\([AMR]\\)\\(!\\)? " nil t)
	(setq conflict (equal "!" (match-string 2))
	      type (match-string 1))
	(if (equal "M" type)
	    ;; Beware of spaces in filename, though there's some
	    ;; ambiguity.  There must be one or two trailing signed
	    ;; integers.  Assume if there are two that the first isn't
	    ;; part of the file name, e.g.
	    ;; ./a -1 +1
	    ;; is actually the file `./a', not `./a -1'.
	    (progn (looking-at "\\(.+?\\)\\(?: [-+][0-9]+\\)\\{1,2\\}$")
		   (setq file (match-string 1)))
	  (setq file (buffer-substring (point) (line-end-position))))
	(vc-file-setprop (expand-file-name file) 'vc-state 'edited)
	;; Fixme:  We need more states for darcs -- see fixme on Emacs
	;; 22 vc-state.  Lets use another property.
	(vc-file-setprop (expand-file-name file) 'darcs-state
			 (cond (conflict 'conflict)
			       ((equal "A" type) 'added)
			       ;; Won't actually get R files.
			       ))))))

;; The default has the uname instead of `modified'.
(defun vc-darcs-dired-state-info (file)
  "VC darcs backend for `dired-state-info'."
  (when (eq 'edited (vc-state file))
    (let ((state (vc-file-getprop file 'darcs-state)))
      (cond ((equal state 'added) "(added)")
	    ((equal state 'conflict) "(conflict)")
	    (t "(modified)")))))

;; The default versions currently lose with non-numeric-style
;; versions.  Just make sure these return nil.
(defalias 'vc-darcs-previous-version 'ignore)
(defalias 'vc-darcs-next-version 'ignore)

;; vc-previous-version doesn't support backend handlers in Emacs 21.
;; (next-version doesn't exist there.)
;; Fixme:  Can the advice be postponed somehow until needed?
(unless (fboundp 'vc-default-previous-version)
(defadvice vc-previous-version (around darcs nil activate)
  "Allow for VC backend functions."
  (let ((function (vc-find-backend-function (vc-backend (buffer-file-name))
					    'previous-version)))
    (if function
	(setq ad-return-value
	      ;; (Single-arg) handlers are defined in Emacs 21, even
	      ;; though vc-previous-version doesn't use them.
	      (funcall function (ad-get-arg 0)))
      ad-do-it))))

(defun vc-darcs-delete-file (file)
  "Delete FILE and delete it in the darcs repository."
  (condition-case ()
      (delete-file file)
    (file-error nil))
  (vc-darcs-command "record" nil 0 file "-m" (concat file "deleted.") ))

(defun vc-darcs-rename-file (old new)
  "Rename file from OLD to NEW using `darcs mv'."
  (vc-darcs-command "mv" nil 0 new old))

;; Fixme:  Fix VC for this.
(unless (fboundp 'vc-default-version-backup-file-name)
(defadvice vc-version-backup-file-name (around darcs nil activate)
  "Allow for VC backend functions."
  (if (vc-find-backend-function (vc-backend (ad-get-arg 0))
				'version-backup-file-name)
      (setq ad-return-value
	    (vc-call version-backup-file-name (ad-get-arg 0) (ad-get-arg 1)
		     (ad-get-arg 2) (ad-get-arg 3)))
    ad-do-it)))

(autoload 'timezone-make-date-sortable "timezone")

(defun vc-darcs-make-iso-time (time)
  "Convert RFC822-like TIME to ISO8601 version."
  (replace-regexp-in-string
   ":" ""
   (timezone-make-date-sortable time nil "UTC")))

(defun vc-darcs-version-backup-file-name (file &optional rev manual regexp)
  "VC darcs backend for `version-backup-file-name'.
Ensure REV is shorter than 21 characters for sanity, hoping that will
be unique.  If REV contains a date specification as from
`log-view-current-tag', return the condensed version of that.
Otherwise return the first 20 characters of REV's md5 hash."
  (if regexp
      ;; Can have anything between the ~s, c.f. CVS.
      (concat (regexp-quote (file-name-nondirectory file))
              "\\.~.+" (unless manual "\\.") "~\\'")
    (unless rev (setq rev (vc-workfile-version file)))
    (if (string-match "\\<date \\(\".+\"\\)" rev)
	(setq rev
	      (vc-darcs-make-iso-time (match-string 1 rev)))
      (when (> (length rev) 20)
	(if (string-match "&&" rev)
	    (setq rev (md5 rev)))	; for want of a better idea
	(setq rev (substring rev 0 20)))
      ;; Slashes in the revision name would screw us and tildes might
      ;; cause confusion.
      ;; Fixme:  Perhaps replace more characters.
      (setq rev (replace-regexp-in-string "[/~]" "-" rev)))
    (expand-file-name (concat (file-name-nondirectory file)
                              ".~" (or rev (vc-workfile-version file))
                              (unless manual ".") "~")
                      (file-name-directory file))))

(defvar vc-darcs-annotation-table nil
  "Internal use.")
(make-variable-buffer-local 'vc-darcs-annotation-table)

;; Fixme:  Maybe use VERSION.
;; Fixme:  The annoatation isn't necessarily of the form assumed here.
;;         We can also have bits of diff-like output.
;;         See http://bugs.darcs.net/issue25
(defun vc-darcs-annotate-command (file buffer &optional version)
  "Prepare BUFFER for `vc-annotate' on FILE.
Each line is tagged with a 14-digit time string YYYYMMDDHHMMSS which
has a `help-echo' property containing author and revision name
information."
  (vc-darcs-command "annotate" buffer 0 file)
  (with-current-buffer buffer
    ;; Store the tags for the annotated source lines in a hash table
    ;; to allow saving space by sharing the text properties.
    (setq vc-darcs-annotation-table (make-hash-table :test 'equal))
    (goto-char (point-min))
    (while (re-search-forward "^# Following line added by [[]\\([^\n]*\\)
# \\(.+\\)\\*\\*\\([^]\n]+\\)\\(]\\)? *$" nil t)
      (let ((start (match-beginning 0))
	    (name (match-string 1))
	    (author (match-string 2))
	    (time (match-string 3))
	    (ended (match-beginning 4))
	    (key (match-string 0))
	    end tag)
	(unless ended (re-search-forward "^] *" nil 'move))
	(setq end (line-beginning-position 2)
	      tag (gethash key vc-darcs-annotation-table))
	;; We could, perhaps, lose the first and last two digits of
	;; the time (two-digit years, and no seconds) to shorten the
	;; tag, e.g. by adding invisibility.
	(unless tag
	  (unless (string-match "[0-9]\\{14\\}" time)
	    ;; Older patches have rfc822-like date strings, not
	    ;; YYYYMMDDHHMMSS.
	    (setq time (vc-darcs-make-iso-time time)))
	  (setq tag (propertize time 'help-echo (concat author ": " name)
				'mouse-face 'highlight))
	  (puthash key tag vc-darcs-annotation-table))
	(delete-region start end)
	(goto-char start)
	(insert tag)))
    ;; Lose initial `created by' stuff.
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^[0-9]\\{12\\}" nil t)
      (kill-region (point-min) (match-beginning 0)))
    (flush-lines "^#" (point-min) (point-max))
    ;; This works in Emacs 22 and 23 to give the equivalent of
    ;; tooltips for the help-echo text in tty mode.  It doesn't do the
    ;; trick in Emacs 21 -- see below.
    (vc-annotate-mode)
    (unless (display-graphic-p)
      (add-hook 'post-command-hook 'vc-darcs-annotate-mode-hook-1 nil t))))

;; This works as above in Emacs 21.  I should put this sort of thing
;; into a global minor mode.
(if (= emacs-major-version 21)
    (add-hook 'vc-annotate-mode-hook 'vc-darcs-annotate-mode-hook))

(defun vc-darcs-annotate-mode-hook ()
  "Set up `post-command-hook' to echo annotation help text."
  (if (and (not (display-graphic-p))
	   (eq 'DARCS vc-annotate-backend))
      (add-hook 'post-command-hook 'vc-darcs-annotate-mode-hook-1 nil t)))

(defun vc-darcs-annotate-mode-hook-1 ()
  "Darcs annotate `post-command-hook' function."
  (let ((help-echo (get-text-property (point) 'help-echo)))
		 (if help-echo (message "%s" help-echo))))

;; Definition from Emacs 22
(eval-and-compile
(unless (fboundp 'vc-annotate-convert-time)
(defun vc-annotate-convert-time (time)
  "Convert a time value to a floating-point number of days.
The argument TIME is a list as returned by `current-time' or
`encode-time', only the first two elements of that list are considered."
  (/ (+ (* (float (car time)) (lsh 1 16)) (cadr time)) 24 3600))))

(defun vc-darcs-annotate-time ()
  (when (re-search-forward "^\\([0-9]\\{14\\}\\) " nil t)
    (let ((time (match-string 1)))
      (vc-annotate-convert-time
       (encode-time (string-to-number (substring time 12 14))
		    (string-to-number (substring time 10 12))
		    (string-to-number (substring time 8 10))
		    (string-to-number (substring time 6 8))
		    (string-to-number (substring time 4 6))
		    (string-to-number (substring time 0 4)))))))

(defun vc-darcs-annotate-extract-revision-at-line ()
  "Return revision for current line of annotation buffer, or nil.
The revision is of the form
\"date <date> && author <author> && exact <name>\".
Return nil if current line isn't annotated."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[0-9]\\{14\\}")
	(let ((date (match-string-no-properties 0))
	      (help-echo (get-text-property (point) 'help-echo)))
	  (if (and help-echo
		   (string-match "\\`\\(.+\\): \\(.+\\)\\'" help-echo))
	      (format "date %s && author %S && exact %S"
		      date (match-string-no-properties 1 help-echo)
		      (match-string-no-properties 2 help-echo)))))))

;; Not needed for Emacs 22
(defun vc-darcs-annotate-difference (point)
  (let ((next-time (vc-darcs-annotate-time)))
    (if next-time
	(- (vc-annotate-convert-time (current-time)) next-time))))

(defun vc-darcs-find-revision (file rev buffer)
  (if (vc-darcs-feature 'show-contents)
      (if (and rev (not (equal rev (vc-darcs-workfile-version file))))
	  (apply #'vc-darcs-command "show" buffer 0 file "contents"
		 (vc-darcs-rev-to-flags rev))
	(vc-darcs-command "show" buffer 0 file "contents"))
    (if (or (not rev) (string= rev vc-darcs-working-name))
	(insert-file-contents (vc-darcs-current file))
      (let ((tmp (make-temp-file "darcs"))
	    (tmp2 (make-temp-file "darcs")))
	(condition-case data
	    (with-current-buffer buffer
	      (copy-file (vc-darcs-current file) tmp t)
	      (vc-darcs-diff file rev nil (current-buffer))
	      (unless (equal 0
			     (call-process-region 1 (point-max)
						  vc-darcs-patch-program
						  t t nil
						  "-t" "-o" tmp2))
		(signal 'error (list (buffer-string))))
	      (insert-file-contents tmp2))
	  (error (error "Patching failed: %S" data)))
	(delete-file tmp)
	(delete-file tmp2)))))

(defun vc-darcs-log-edit-setup ()
  "Make first line of buffer separate paragraphs in darcs log-edit buffer.
Intended to be added to `log-edit-mode-hook' to avoid filling the
long comment onto the patch name."
  (when (eq 'DARCS (vc-backend (buffer-file-name)))
    ;; This doesn't seem to work in Emacs 21 for reasons I haven't
    ;; checked on.
    (set (make-local-variable 'paragraph-separate)
	 (concat "\\`.*$\\|" paragraph-separate))))

(add-hook 'log-edit-mode-hook 'vc-darcs-log-edit-setup)

;; In case of just `(load "vc-darcs")', but that's probably the wrong
;; way to do it.
;;;###autoload
(add-to-list 'vc-handled-backends 'DARCS)

;;;###autoload
(eval-after-load "vc"
  ;; No-op in Emacs 23:
  '(add-to-list 'vc-directory-exclusion-list "_darcs" t))

(defconst vc-darcs-unload-hook		; Emacs 21
  (lambda ()
    (dolist (function '(log-view-current-tag vc-previous-version
			vc-version-backup-file-name))
      (condition-case ()		; in case advice is missing
	  (ad-remove-advice function 'around 'darcs)
	(error nil)))
    (remove-hook 'log-edit-mode-hook 'vc-darcs-log-edit-setup)
    (remove-hook 'vc-annotate-mode-hook 'vc-darcs-annotate-mode-hook)
    (setq vc-handled-backends (delq 'DARCS vc-handled-backends))))

(defalias 'vc-darcs-unload-function vc-darcs-unload-hook) ; Emacs 22

;; Emacs 23 stuff

(defun vc-darcs-revision-granularity ()
  'repository)

(eval-when-compile (defvar log-view-patch-based))

(define-derived-mode vc-darcs-log-view-mode log-view-mode "Darcs-Log-View"
  ;; Fixme:  Make `log-view-diff' work with darcs log format -- needs
  ;; a handler for `log-view-current-tag' in log-view.el.  In the
  ;; meantime, see the advice below.  Also, it needs some sort of
  ;; handler to make it work with logs of patches so that diffing
  ;; doesn't always work on two patches.
  (require 'add-log)
  (set (make-local-variable 'log-view-file-re)
       "^Changes to \\(.+\\):$")
  (set (make-local-variable 'log-view-message-re)
       "^\\(\\(?:\\S-+[ \t]+\\)\\{5\\}\\S-+\\)[ \t]+")
  (set (make-local-variable 'log-view-font-lock-keywords)
       `((,log-view-file-re (1 'log-view-file-face))
	 (,log-view-message-re
	  (0 'change-log-date-face)
	  ;; Adapted from add-log.
	  ("\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9.-]+\\)[>)]\
\\|\\(.*$\\)"
	   nil nil
	   (1 'change-log-name-face nil t)
	   (2 'change-log-email-face nil t)
	   (3 'change-log-email-face nil t)))
	 ("^  \\* \\(.*\\)$" (1 'log-view-message-face))
	 ;; Prefixes `tagged ' and `UNDO: ' don't occur in the output
	 ;; for a single file -- I wonder if they should -- but we
	 ;; might want to use this in darcsum, for instance.
	 ("^  \\(\\(?:tagged\\|UNDO:\\) .*\\)$" (1 'log-view-message-face))))
  (set (make-local-variable 'log-view-current-tag-function)
       #'vc-darcs-log-view-current-tag)
  (set (make-local-variable 'log-view-patch-based) t) ; for fixed log-view-diff
  (vc-exec-after
   '(when vc-darcs-print-log-summary
      ;; Remove summary lines that don't refer to this file.
      (save-excursion
	(goto-char (point-min))
	(let (case-fold-search)
	  (while (re-search-forward "^    \\(?:[MAR] \\./\\| .+ -> \\)" nil t)
	    (beginning-of-line)
	    (unless (looking-at
		     (concat ".+/" (regexp-quote (file-name-nondirectory
						  (log-view-current-file)))
			     "\\(?: \\|$\\)"))
	      (delete-region (line-end-position -1) (line-end-position)))
	    (end-of-line)))))))

(defun vc-darcs-create-repo ()
  "Initialize the current directory as a Darcs repo."
  (vc-darcs-command "initialize" nil 0 nil))

(defun vc-darcs-create-tag (dir name branchp)
  (if branchp
      ;; The branch seems to have to be in-place.
      (error "Can't branch when tagging with Darcs"))
  (vc-darcs-command "tag" nil 0 nil "--repodir" dir "--skip-long-comment"
		    name))

;; By comparison with other backends, this should be recursive.
;; Unless we remove directory entries, they get duplicated in vc-dir
;; buffers.
(defun vc-darcs-dir-status (dir update-function)
  (vc-darcs-dir-status-files dir nil 'up-to-date update-function))

(defun vc-darcs-dir-status-files (dir files default-state update-function)
  ""
  (let ((files (mapcar (lambda (file) (file-relative-name file dir))
		       files))
	(top (vc-darcs-root dir)))
    ;; Could use --ignore-times.
    (apply #'vc-darcs-command "whatsnew" t 'async nil "-l" "--summary" files)
    (vc-exec-after
     `(vc-darcs-after-dir-status ,dir ,top ,update-function ,files))))

(defun vc-darcs-after-dir-status (dir root update-function files)
  "Process `vc-darcs-dir-status-files' buffer, run by `vc-exec-after'."
  (goto-char (point-min))
  (let (result this)
    (while (not (eobp))
      (let ((this (vc-darcs-whatsnew-state)))
	(cond ((null this))
	      ((listp (car this))
	       (push (car this) result)
	       (push (cdr this) result))
	      (t (push this result))))
      (forward-line))
    (let ((levels (vc-darcs-directory-levels dir)) result2)
      (dolist (elt result)
	(when elt
	  (let ((file (expand-file-name (car elt) root)))
	    (unless (or
		     (file-directory-p file)
		     ;; Filter out entries between ROOT and DIR.  [With
		     ;; POSIX names we could just look for a ".."
		     ;; prefix in `(file-relative-name file dir)'.]
		     (< (vc-darcs-directory-levels file) levels))
	      (push (cons (file-relative-name file dir) (cdr elt))
		    result2)))))
      ;; A file missing from the list is up-to-date.
      (dolist (file (mapcar #'file-relative-name files))
	(unless (assoc file result)
	  (push (list file 'up-to-date nil) result2)))
      (funcall update-function result2 ))))

;; Fixme: Is this guaranteed to terminate sensibly with non-POSIX
;; pathnames, in which case the defensive check can be removed?
(defun vc-darcs-directory-levels (filename)
  "Count the number of directory levels in FILENAME below the root."
  (let ((n 0)
	(last (file-name-directory filename)))
    (while (and last (not (equal last (file-name-directory
				       (directory-file-name last)))))
      (setq n (1+ n)
	    last (file-name-directory (directory-file-name last)))
      ;; Defend against an infinite loop in case this doesn't work for
      ;; non-POSIX pathnames.
      (if (> n 100)			; arbitrary large number of levels
	  (error "vc-darcs-directory-levels didn't terminate")))
    n))

(defun vc-darcs-whatsnew-state ()
  "Return the VC state for a `whatsnew' output line with point at BOL.
It may be one of the forms
  (FILE STATE nil)
  ((FILE1 'removed nil) . (FILE2 'added nil))
  nil
for use in `vc-darcs-dir-status-files'."
  (let ((case-fold-search nil)
	(conflict (if (looking-at ".!") 'conflict)))
    (cond
     ;; File names with spaces aren't properly delimited.  There are
     ;; one or two line change counts after the filename.  Assume any
     ;; such we find there isn't part of the name.
     ((looking-at "M!? \\(.+?\\)\\(?: [-+][0-9]+\\)\\{1,2\\}$")
      (list (match-string 1) (or conflict 'edited) nil))
     ((looking-at "A!? \\(.*\\)$")
      (list (match-string 1) (or conflict 'added) nil))
     ((looking-at "R!? \\(.*\\)$")
      (list (match-string 1) (or conflict'removed) nil))
     ((looking-at "a!? \\(.*\\)$")
      (list (match-string 1)
	    (or conflict 'unregistered)	; don't know if conflict is possible
	    nil))
     ((looking-at " \\(.*\\) -> \\(\\..*\\)$")
      (cons (list (match-string 1) 'removed nil)
	    (list (match-string 2) 'added nil)))
     ((looking-at "WARNING: File '\\(.*\\)' \
\\(?:\\(does not exist\\)\\|\\(not in repository\\)\\)!$")
      ;; See if the file is actually add-able.  (See comment about
      ;; ignored files in `vc-darcs-state'.)
      (let ((file (regexp-quote (match-string 1))))
	(save-excursion
	  (unless (re-search-forward (concat "^a!? .." file "$") nil t)
	    (list file 'ignored nil))))))))

(defun vc-darcs-dir-extra-headers (dir)
  (with-temp-buffer
    (let* ((dir (expand-file-name dir))
	   (remote (vc-darcs-repository-hostname dir))
	   format)
      (when (vc-darcs-feature 'show-repo)
	(vc-darcs-command "show" t 0 nil "repo" "--repodir" dir)
	(goto-char 1)
	(if (save-excursion
	      (re-search-forward " *Format: \\(.*\\)$" nil t))
	    (setq format (match-string 1))))
      (concat
       (propertize "Repository : " 'face 'font-lock-type-face)
       (propertize (vc-darcs-root dir) 'face 'font-lock-variable-name-face)
       "\n"
       (if remote
	   (concat (propertize "Remote repo: " 'face 'font-lock-type-face)
		   (propertize remote 'face 'font-lock-variable-name-face)
		   "\n"))
       (if format
	   (concat (propertize "Repo format: " 'face 'font-lock-type-face)
		   (propertize format 'face 'font-lock-variable-name-face)
		   "\n"))))))

;; Not sure if this is actually useful except above.
(defun vc-darcs-repository-hostname (dir)
  (when (vc-darcs-feature 'show-repo)	; not sure about darcs1
    (with-temp-buffer
      (vc-darcs-command "show" t 0 nil "repo" "--repodir" dir)
      (goto-char 1)
      (if (re-search-forward "^ *Default Remote: \\(.*\\)$" nil t)
	  (match-string 1)))))

(defun vc-darcs-previous-revision (file rev)
  (condition-case ()
      (with-temp-buffer
	(vc-darcs-get-changes t 0 file "--max-count=2" "--no-summary"
			      (vc-darcs-rev-to-flags rev 'to))
	(goto-char 1)
	(forward-line)			; skip "Changes to ..."
	(when (re-search-forward "^[^\n ]" nil t 2) ; we have two headers
	  (beginning-of-line)
	  (let* ((date (progn (looking-at "\\w+\\(?: [^ ]+\\)\\{5\\}")
			      (vc-darcs-make-iso-time (match-string 0))))
		 (name (progn (forward-line)
			      (looking-at "  \\* \\(.+\\)$")
			      (match-string 1))))
	    (format "date %s && exact %S" date name))))
    (error nil)))

(defun vc-darcs-next-revision (file rev)
  (condition-case ()
      (with-temp-buffer
	(vc-darcs-get-changes t 0 file "--max-count=2" "--no-summary"
			      (vc-darcs-rev-to-flags rev 'from))
	(goto-char 1)
	(forward-line)			; skip "Changes to ..."
	(when (save-excursion
		(re-search-forward "^[^ \n]" nil t 2)) ; we have two headers
	  (let* ((date (progn (re-search-forward "^\\w+\\(?: [^ ]+\\)\\{5\\}")
			      (vc-darcs-make-iso-time (match-string 0))))
		 (name (progn (forward-line)
			      (looking-at "  \\* \\(.+\\)$")
			      (match-string 1))))
	    (format "date %s && exact %S" date name))))
    (error nil)))

;; Fixme:  Add: amend-record (log message only), apply, check,
;; unrecord, mark-conflicts, obliterate?
(defun vc-darcs-extra-menu ()
  (let ((map (make-sparse-keymap)))
    (define-key map [darcs-optimize]
      '(menu-item "Darcs optimize" vc-darcs-optimize
		  :help "Optimize current repo"))
    (define-key map [darcs-repair]
      '(menu-item "Darcs repair" vc-darcs-repair
		  :help "Repair current repo"))
    (define-key map [darcs-replace]
      '(menu-item "Darcs replace" vc-darcs-replace
		  :help "Replace token in current file"))
    map))

;; Fixme:  Allow extra args.
(defun vc-darcs-optimize ()
  "Run `darcs optimize' in the current repository."
  (interactive)
  (vc-darcs-command "optimize" nil 0 nil))

(defun vc-darcs-repair ()
  "Run `darcs repair' in the current repository."
  (interactive)
  (vc-darcs-command "repair" nil 0 nil))

;; Fixme:  This should probably allow a file (list).
(defun vc-darcs-replace (old new)
  "Use `darcs replace' to replace OLD with NEW in buffer's file.
Re-visit file afterwards."
  (interactive "MReplace token: \nMWith: ")
  (vc-buffer-sync)
  ;; -f is probably sensible.
  (vc-darcs-command "replace" nil 0 (buffer-file-name) "-f" old new)
  ;; Re-read, and go as close as possible to where we were.
  (let ((point (point)))
    (find-alternate-file (buffer-file-name))
    (goto-char point)))

(provide 'vc-darcs)
;;; vc-darcs.el ends here
