;;; ultex.el --- Ultra-TeX mode
;; Copyright (c) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;; 2000, 2001, 2002
;; Mark Haiman, Nick Reingold, John Palmieri

;; Authors:   Mark Haiman <mhaiman@macaulay.ucsd.edu>, 
;;            Nick Reingold, 
;;            John Palmieri <palmieri@math.washington.edu>
;; Maintainer: John Palmieri <palmieri@math.washington.edu>
;;             URL: http://www.math.washington.edu/~palmieri/Emacs/ultratex.html
;; Keywords: TeX-mode, completion
;; Version:  0.71 of Wed Jun  5 22:19:04 PDT 2002

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: 
;; 
;; Ultra TeX mode is a major mode for editing TeX documents.  Typing
;; C-h m while in Ultra-TeX-mode will display information on the
;; mode.
;;
;; Main features: 
;;  1. Completion of control sequences.
;;    Typing a backslash (\) starts lightning completion (see light.el) of 
;;    control sequences.  The ultex-tree-file is the file of permanent 
;;    control sequences to complete on.  Also, when you find a TeX file, 
;;    control sequences defined in that file are added to the list of things to
;;    complete on.  These sequences are not added to the permanent file,
;;    however.  To add a control sequence to the permanent file, use C-c C-n
;;    (M-x ultex-new-cs).  You can also edit the ultex-tree-file directly.
;;    The permanent completion list may have sequences which do not begin
;;    with a \.  To complete on any sequence, including those, type C-\.
;;
;;  2. Greek keyboards.
;;    Since there are many symbols which have TeX control sequence names, 
;;    it is undesirable to clutter up the completion list with these.  
;;    Instead, several Greek Keyboards are provided in Ultra TeX mode.  
;;    The key ` acts as a "Greek shift" key.  Typing `a will insert
;;    "\alpha" in the buffer (assuming you do not change the default 
;;    greek key bindings).  There are Greek, Shift Greek, Control ^Greek,
;;    Meta Greek, Meta Shift Greek, and Meta Control Greek keyboards.  You 
;;    can add or change a greek key by typing C-c C-g in ultra-tex-mode.
;;    You can display the current greek key bindings by typing `\, `|, `C-\,
;;    `M-\, `M-|, and `M-C-\.  
;;    The greek bindings are stored in the file ultex-greek-keys-file.
;;
;;  3. Fonts.
;;    Typing ESC B will insert "\textbf{}" or "\mathbf{}" (whichever is
;;    appropriate, depending on the location of the point) and leave point
;;    just inside the group.  Similarly, ESC A,C,E,F,I,M,R,S,T,X,Z make
;;    cAligraphic, smallCaps, Emphasis, sans-seriF, Italic, Math-italic,
;;    Roman, Slant, Typewriter, teXt and uZer's-math (defined as \zz) fonts.
;;
;;  4. Smart Return Key
;;    The RETURN key moves point forward by some appropriate amount.  Often
;;    this means moving outside of a close bracket, or over a whole word.  
;;    Also, RET can remove some extraneous matter.  For instance, if the 
;;    text looks like "[]" and the cursor is between the two brackets, 
;;    hitting RET deletes the pair (and similarly if the cursor is between 
;;    the braces in "\label{}").  This way, one can use lightning 
;;    completion to insert a command with lots of optional arguments, and 
;;    one only has to hit RET to get rid of each one.
;;    To insert a newline, use ESC RET.  LFD ends a paragraph when that makes
;;    sense, and checks the paragraph to see if parentheses, etc. are properly
;;    matched.
;;
;;  5. Smart Dollar Signs.
;;    Typing $ produces two dollar signs and puts the point in between them.
;;    Typing $$ makes a displayed equation.  Repeated $ cycles among the 
;;    following (here, @=point)
;;      $$
;;      @
;;      $$
;;
;;      \[
;;      @
;;      \]
;;
;;      \begin{equation}\label{@}
;;      
;;      \end{equation}
;;      
;;      \begin{equation*}
;;      @
;;      \end{equation*}
;; 
;;      etc.
;;    You get the same behavior whether there is text between the
;;    starting and ending delimiters:
;;      \[
;;      x=y
;;      \]
;;    becomes
;;      \begin{equation}\label{}
;;      x=y
;;      \end{equation}
;;
;;  6. Miscellaneous.
;;    C-c C-s inserts a basic LaTeX outline from file 
;;    ultex-latex-skel-file, then runs ultex-skel-hook. 
;;
;;    { makes a brace pair and leaves point inside.
;;
;;    " makes `` or ''. 
;;
;;    ... makes \dots.
;;
;;    ^ and _ include a brace pair.  
;;
;;    TAB in array environments inserts &.
;;
;;    \\ will insert a newline and make new \item when that is
;;    appropriate, otherwise, just inserts \\ and a newline.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version history
;;
;; 0.71 (05-Jun-2002) GNU Emacs 21 fixes, other small things.
;; 0.70 (25-Oct-2001) Bind C-\ to do something useful.
;; 0.69 (14-May-2001) Added a bunch of little fixes: new customizable
;;          variables ultex-subscript-delete-leading-space and
;;          ultex-item-insert-brackets, made " key work better, tried
;;          to improve placement of point after inserting missing math
;;          delimiter.
;; 0.68 (09-Feb-2001) New variable: ultex-version.
;; 0.65 (03-Aug-2000) Search for input and include files using the
;;          TEXINPUTS environment variable.
;; 0.64 (09-Aug-1999) Grab bibs recursively.  Also, do not grab
;;          newcommands (etc.) which do not start at the beginning of
;;          the line.
;; 0.63 (29-Jun-1999) Changed my e-mail address, URL.
;; 0.62 (11-May-1999) ultex-insert-quotes: hitting " twice inserts ".
;;          Also incorporated new stuff from latex-symbols 0.02.
;; 0.61 (10-May-1999) Made a bit more compatible with Emacs 19
;;          (removed `unless').
;; 0.60 (29-Apr-1999) Separated ultra-tex from light, latex-symbols
;;          packages.  (Since light is required by ultex, light is
;;          distributed with it.  light is also distributed
;;          separately, in case anyone just wants it rather than all
;;          of the ultra-tex stuff.)
;; 0.55 (24-Apr-1999) Rewrote greek key stuff to allow key binding
;;          commands with syntax like '[(meta ?a)], etc.  Added
;;          function ultex-ret-add-command.  Added option to display
;;          greek keys as pictures instead of text, and to show tables
;;          of symbols.  Fixed Ultra-TeX menu in XEmacs.
;; 0.54 (12-Feb-1999) Version number increased to keep up with light.el
;; 0.53 (26-Jan-1999) Changed roles of ultex-permanent-alist and
;;          ultex-current-commands: the former is what is read from
;;          the various textree files, while the latter contains
;;          only commands scanned from the current file.
;;          (ultex-current-commands used to be ultex-permanent-alist,
;;          modified by the scanning of the current file.)
;; 0.52 (19-Jan-1999) Bug fix in Greek meta keys and XEmacs.
;; 0.51 (14-Jan-1999) Version number increased to keep up with light.el
;; 0.50 (21-Dec-1998) Added ultex-extra-tree-files, ultex-also-load,
;;          ultex-delete-csname-from-tree-file, cleaned up some bugs
;;          with greek keys, added a menu, removed support for GNU
;;          Emacs 18.
;; 0.41 (28-Sep-1998) Fixed bug with SPC in greek keyboard.
;; 0.40 (25-Sep-1998) Modified definition grabbing so that if you set
;;          the variable TeX-master to a file, that file is scanned in
;;          addition to everything else.
;; 0.39 (03-Sep-1998) Version number increased to keep up with light.el
;; 0.38 (28-Apr-1998) Fixed bug: ultex-make-ref-from-label was
;;          rescanning ultex-current-labels from the current buffer,
;;          not from any included files.
;; 0.37 (17-Apr-1998) Fixed bug re: inserting Greek meta keys.
;; 0.36 (07-Apr-1998) Replaced ultex-grab-commands and
;;          ultex-grab-labels with a single function, ultex-grab.
;;          Also fixed bug in ultex-grab-bibs that prevented it from
;;          working correctly when given an optional argument, if
;;          ultex-alist-local-flag was non-nil.
;; 0.35 (27-Mar-1998) ultex-grab-defs now works recursively on all
;;          input and include files.  Fixed another bug in
;;          ultex-grab-commands, ultex-grab-labels.  Added
;;          \DeclareMathSymbol to ultex-grab-commands.  Fixed bug in
;;          ultex-grab-next that prevented it from finding \newcommand
;;          (etc.) right at the start of a buffer.  Made
;;          ultex-tab-resume resume completion better.
;; 0.34 (23-Mar-1998) Fixed bug in ultex-grab-commands, ultex-grab-labels.
;; 0.33 (31-Oct-1997) Changed behavior of $ after \.
;; 0.32 (02-Sep-1997) Version number increased to keep up with light.el
;; 0.31 (01-Aug-1997) Bug fix: in ultex-grab-bibs, got error if the
;;          buffer was not associated to a file.
;; 0.30 (30-Jul-1997) Changed version number when distribution was
;;          reorganized.
;; 0.26 (23-Jul-1997) Fixed a problem with ultex-make-ref-from-label.
;; 0.25 (27-Feb-1997) The keymap stuff was rewritten a bit, changing
;;          the way greek keys are handled (internally--not from the
;;          point of the view of the user).
;;             New variable: ultex-dont-use-completion.  Set this to
;;           be non-nil if you don't want to use lightning
;;           completion.  (This replaces the function
;;           ultex-dont-use-completion.)
;;             New variables: ultex-match-braces,
;;           ultex-match-brackets, ultex-match-parens.  If these are
;;           non-nil, then hitting `{' (or `[' or `(') inserts a
;;           matched pair of the delimiters, with the cursor in
;;           between.
;; 0.24 (14-Feb-1997) Replaced ultex-current-alist with
;;           ultex-current-commands, ultex-current-labels,
;;           ultex-current-bibs, so one can rescan for only certain
;;           kinds of commands, rather than rescan for everything. 
;;           Lots of other things had to be changed a little to
;;           reflect this.
;;             New variable: ultex-rescan-after-new-label.  When
;;           non-nil, then rescan the buffer for labels after each new
;;           label is entered.
;;             New function: ultex-dont-use-completion.  Put this in
;;           your ultra-tex-mode-hook if you don't want to use
;;           lightning completion.
;; 0.23 (16-Jan-1997) biggest change: the dollar sign.  Now hitting $
;;           while in math mode changes the starting and ending
;;           strings of math mode--see examples above.
;;             screwed around with ultex-grab-defs, etc., in an
;;           attempt to make the code a little less unpleasant.
;;             changed ultex-math-mode so that it would ignore \\[0.5in]
;;           and things like that, and also so that it doesn't beep
;;           when you're entering arguments to \newcommand.
;;             added lightning completion on TeX commands to
;;           ultex-delete-csname.
;;             changed values of paragraph-ignore-fill-prefix and
;;           adaptive-fill-mode in order to make comments and
;;           auto-fill-mode interact with each other better.  Also
;;           changed value of comment-start-skip.
;; 0.2 (28-Oct-1996) started numbering the versions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'light)

(require 'ultex-cus)

(defconst ultex-version-string "0.71"
  "Version ultra-tex package.")

(defconst ultex-version ultex-version-string
  "Version ultra-tex package.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoloads
;;

(autoload 'latex-symbol-greek "latex-symbols"
  "Pop up window showing a table of Greek letters.")

(autoload 'latex-symbol-operators "latex-symbols"
  "Pop up window showing a table of binary operators (like times, wedge, ...).")

(autoload 'latex-symbol-relations "latex-symbols"
  "Pop up window showing a table of binary relations (like cong, sim, ...).")

(autoload 'latex-symbol-arrows "latex-symbols"
  "Pop up window showing a table of arrows.")

(autoload 'latex-symbol-miscellany "latex-symbols"
  "Pop up window showing a table of miscellaneous symbols.")

(autoload 'latex-symbol-all "latex-symbols"
  "Pop up window showing a table of many (La)TeX symbols.")

(autoload 'ls-display-picture "latex-symbols"
  "Display image in PICTURE-FILE.  Add PAD spaces, some before, some
after.  Display in buffer BUF.  Return the width of the image, so one
can fix bad spacing.")

(autoload 'latex-symbol-plain "latex-symbols"
  "Pop up window showing a table of symbols defined in plain (La)TeX.")

(autoload 'latex-symbol-latexsym "latex-symbols"
  "Pop up window showing a table of symbols defined in latexsym.sty.")

(autoload 'latex-symbol-amsfonts "latex-symbols"
  "Pop up window showing a table of symbols defined in amsfonts.sty.")

(autoload 'latex-symbol-amsmath "latex-symbols"
  "Pop up window showing a table of symbols defined in amsmath.sty.")

(autoload 'latex-symbol-amssymb "latex-symbols"
  "Pop up window showing a table of symbols defined in amssymb.sty.")

(autoload 'latex-symbol-amsthm "latex-symbols"
  "Pop up window showing a table of symbols defined in amsthm.sty.")

(autoload 'latex-symbol-stmaryrd "latex-symbols"
  "Pop up window showing a table of symbols defined in stmaryrd.sty.")

(autoload 'ls-lookup-symbol "latex-symbols"
  "Given string STR (such as \"alpha\" or \"longrightarrow\"), return
list of packages in which that symbol is defined.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defvars
;;

(defvar ultra-tex-mode-abbrev-table nil
  "Abbrev table used while in ultra-tex mode.")

(defvar ultra-tex-mode-syntax-table nil
  "Syntax table used while in ultra-tex mode.")

;; This is a cons so it can be changed by side effect.
(defvar ultex-permanent-alist '(nil)
  "List of TeX commands, scanned from ultex-tree-file")

(defvar ultex-current-commands nil
  "List of TeX commands on which to complete.  Initially set to nil.
Modified based on contents of buffer.")

(defvar ultex-current-labels nil
  "List of \ref (etc.) commands, on which to complete.
Built from \label entries in buffer.")

(defvar ultex-current-bibs nil
  "List of \cite commands on which to complete.
Built from bibliographical entries in buffer.")

(defvar ultex-current-tree-files nil
  "List of tree files used in the current buffer.")

(make-variable-buffer-local 'ultex-current-tree-files)

(defvar ultra-tex-mode-map nil
  "Keymap for ultra-tex-mode")

(defvar ultex-greek-map nil
  "Keymap for Greek keyboard")

(defvar ultex-greek-meta-map nil
  "Keymap for meta Greek keyboard")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; entry point and other main functions.
;;

(defun ultra-tex-mode nil
  "Major mode for editing TeX documents.  

\\<ultra-tex-mode-map>\\[ultex-fast-cs] starts lightning completion of control sequences.  Use \\[ultex-fast-noslash] for those
that don't start with \\.  Resume key is \\[ultex-tab-resume].  \\[ultex-new-cs] makes new permanent
control sequences for completion.  The file ultex-tree-file keeps
permanent control sequences.  You can edit it. It will be saved and
reloaded automatically whenever you complete.  It must be readable as
an alist by the lisp reader.

\\[ultex-greek-map] is the Greek shift key.  Do \\[ultex-greek-map] followed by \\<ultex-greek-map>\\[ultex-show-plain], \\[ultex-show-shift], \\[ultex-show-control], etc., 
for display of ordinary, shift, control, etc. greek keyboard.  Your
ultex-greek-keys-file defines keys in ultex-greek-map.
\\<ultra-tex-mode-map>\\[ultex-redefine-greek-key] makes greek key redefinitions.

ESC A,B,C,E,F,I,M,R,S,T,X,Z make cAligraphic, Boldface, smallCaps,
Emphasis, sans-seriF, Italic, Math-italic, Roman, Slant, Typewriter,
teXt, and uZer's-math (defined as \\zz) fonts.

\\[ultex-over-and-out] moves forward from groupings when that makes sense.  Plain 
newline is \\[newline].  \\[ultex-par] ends paragraph when that makes sense.

\\[get-latex-skel] inserts a basic LaTeX outline from file ultex-latex-skel-file,
then runs ultex-skel-hook.

\\[ultex-insert-braces] makes a brace pair.  \\[ultex-insert-dollars] makes a $ pair.  Repeated \\[ultex-insert-dollars] cycles through 
a number of different options.  \\[ultex-insert-quote] makes `` or ''.  \\[ultex-maybe-dots]\\[ultex-maybe-dots]\\[ultex-maybe-dots] makes various 
\\dots.  \\[ultex-superscript] and \\[ultex-subscript] include a brace pair.  \\[ultex-tab-resume] in array environments 
inserts &.

\\[ultex-fast-cs]\\[ultex-fast-cs] can make new \\item's by hooking ultex-next-item in your
completion list.   

Entry into this mode runs tex-mode-hook and then ultra-tex-mode-hook.
Special functions: 
\\{ultra-tex-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "UltraTeX")
  (setq major-mode 'ultra-tex-mode)
  (setq local-abbrev-table ultra-tex-mode-abbrev-table)
  (if (null ultra-tex-mode-syntax-table)
      (progn
	(setq ultra-tex-mode-syntax-table (make-syntax-table))
	(set-syntax-table ultra-tex-mode-syntax-table)
	(or ultex-blink-matching-paren
	    (progn
	      (modify-syntax-entry ?\( "_")
	      (modify-syntax-entry ?\) "_")))
	(or ultex-blink-matching-bracket
	    (progn
	      (modify-syntax-entry ?\[ "_")
	      (modify-syntax-entry ?\] "_")))
	(modify-syntax-entry ?\\ "\\")
	(modify-syntax-entry ?\$ "$$")
	(modify-syntax-entry ?\" ".")
	(modify-syntax-entry ?& ".")
	(modify-syntax-entry ?_ ".")
	(modify-syntax-entry ?@ "_")
	(modify-syntax-entry ?~ " ")
	(modify-syntax-entry ?% "<")
	(modify-syntax-entry ?\f ">")
	(modify-syntax-entry ?\n ">")
	(modify-syntax-entry ?' "w"))
    (set-syntax-table ultra-tex-mode-syntax-table))
  (make-local-variable 'completing-insert-function)
  (setq completing-insert-function 'ultex-fast-noslash)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start
	"[ \t]*$\\|[\f%]\\|\\$\\$\\|\\\\begin{\\|\\\\end{\\|.*\\\\\\\\[ \t]*$\\|\\\\item\\|\\\\\\[\\|\\\\\\]"
	paragraph-separate
	"[ \t]*$\\|[\f%]\\|\\$\\$[ \t]*$\\|.*\\\\\\\\[ \t]*$\\|\\\\\\[\\|\\\\\\]\\|\\\\end{.*}[ \t]*$\\|\\\\begin{.*}[ \t]*$\\|\\\\begin{.*}\\\\label{.*}[ \t]*$")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(^\\|[^%\\]\\)%+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ultex-comment-indent)
  (if ultex-alist-local-flag
      (progn
	(make-variable-buffer-local 'ultex-permanent-alist)
	(make-variable-buffer-local 'ultex-current-commands)
	(make-variable-buffer-local 'ultex-current-labels)
	(make-variable-buffer-local 'ultex-current-bibs)))
  (or ultra-tex-mode-map
      (progn
	(ultex-define-ultra-tex-mode-map)
	(ultex-reset-greek-keys)))
  (ultex-menu-define-xemacs)
  (use-local-map ultra-tex-mode-map)
  (hack-local-variables)
  (ultex-reset-perm-alist)

  (let ((ultex-start (save-excursion
			    (ultex-re-search-forward
			     "\\\\begin{document}" 
			     (point-max) t))))
    (if (or (search-forward "--ultex--dont--grab--" ultex-start t)
	    (and buffer-read-only
		 (or ultex-alist-local-flag
		     (not (y-or-n-p
			   (format  "Grab TeX commands, bibliographic info from buffer %s? "
					(buffer-name (current-buffer))))))))
	(progn (message "Not grabbing from this buffer.")
	       nil)
      (ultex-grab-defs)
      (ultex-grab-bibs)))
  (ultex-check-styles)
  (if ultex-run-text-mode-hook (run-hooks 'text-mode-hook))
  (run-hooks 'tex-mode-hook
	     'ultra-tex-mode-internal-hook
	     'ultra-tex-mode-hook))

;; completion of control sequences
(defun ultex-fast-cs nil
  "Lightning complete control sequence and argument framework."
  (interactive)
  (insert "\\")
  (setq lc-complete-idle-time ultex-complete-idle-time)
  (completing-insert
   (append ultex-permanent-alist
	   ultex-current-commands
	   ultex-current-labels
	   ultex-current-bibs)
   nil 1 'point-adjust-hook "TeX \\csnames"))

(defun ultex-fast-noslash (&optional init)
  "Lightning complete control sequence, including ones without backslash."
  (interactive)
  (setq lc-complete-idle-time ultex-complete-idle-time)
  (completing-insert
   (append ultex-permanent-alist
	   ultex-current-commands
	   ultex-current-labels
	   ultex-current-bibs)
   nil (or init 0) 'point-adjust-hook "TeX names"))

(defun ultex-tab-resume (&optional init)
  "Resume lightning completion, or just TAB."
  (interactive (list (ultex-command-grabber)))
  (or (and (not (= init 0)) (ultex-fast-noslash init))
      (progn
	(and (memq (latex-environment) ultex-array-environs)
	     (insert "&"))
	(indent-for-tab-command))))

;; I'm not sure about the (if (= (following-char) ...)) stuff.  Maybe
;; this behavior should be controlled by a variable.
(defun ultex-command-grabber nil
  "Move point to just after the command point is in or after, and
return length of command."
  (let* ((word-start "^ \n\t\f\"$`'();~\\\\")
	 (word-end (concat word-start "{}")))
  (skip-chars-forward word-end)
  (if (= (following-char) ?}) (forward-char))
  (- (point) (save-excursion
	       (skip-chars-backward word-start)
	       (if (= (preceding-char) ?\\)
		   (1- (point))
		 (point))))))

(defun ultex-try-to-complete nil
  "Try to complete TeX command sequence just before point, but don't
enter lightning completion mode.  If there is a unique completion,
insert it.  If there are several choices, pop up a new buffer listing
them (but one can't select an option from that buffer).  Etc."
  (interactive)
  (let* ((slash
	  (save-excursion
	    (if (search-backward "\\"
				 (save-excursion
				   (forward-paragraph -1)
				   (point))
				 t)
		(point) nil)))
	 (grab (if slash
		   (buffer-substring-no-properties slash (point))
		 ""))
	 (n 0))
    (if slash
	(progn
	  (setq lc-stack nil)
	  (while (<= n (length grab))
	    (setq lc-stack (cons (substring grab 0 n) lc-stack))
	    (setq n (1+ n)))
	  (setq lc-table (append ultex-permanent-alist
				 ultex-current-commands
				 ultex-current-labels
				 ultex-current-bibs)
		lc-predicate nil
		lc-cycle nil
		lc-hook 'point-adjust-hook)
	  (let ((old (car lc-stack))
		(top (lc-complete-stack-top "")))
	    (cond ((string= old top)
		   (let ((lc-completion-message-function
			  'ultex-completion-message-function))
		     (lc-display-completions)))
		  ((eq top t)
		   (setq lc-prev-windows nil)
		   (lc-quit t)
		   (delete-windows-on (get-buffer "*Completions*")))
		  ((and (null top)
			(memq (latex-environment) ultex-array-environs))
		   (insert "&")
		   (indent-for-tab-command))
		  ((null top)
		   (ding)
		   (lc-message "No match.")))))
      (and (memq (latex-environment) ultex-array-environs)
	   (insert "&"))
      (indent-for-tab-command))))

(defun ultex-completion-message-function ()
  "Message function for use with TAB key (or whatever) in ultra-tex mode."
  nil)

(defun ultex-new-cs (&optional arg)
  "Enter a new lightning control sequence.

Pop up a buffer, in which user edits control sequence, leaving point
at the desired position.  Exit by hitting M-C-c, at which time
control-sequence is added to the permanent file and the current alist.
Prompt to see which tree file to store the command, if the current
file is using more than one.  Prompt to add hook to warn when used
outside of math mode.

Optional argument ARG is there for backwards compatibility---it does
nothing."
  (interactive)
  (let (s n x c treebuf temp file)
    (save-window-excursion
      (pop-to-buffer " *TeX Completions Edit*")
      (erase-buffer)
      (insert "\\")
      (message "Make new completion, place point, then exit (ESC C-c).")
      (recursive-edit)
      (setq x (y-or-n-p "Use in math mode exclusively? "))
      (setq s (buffer-substring-no-properties (point-min) (point-max)))
      (setq n (- (point) (point-max))))
    (setq c (cons s (cons n (if x 'ultex-math-mode))))
    ;; get tree file name
    (cond 
     ((zerop (length ultex-current-tree-files)) ; shouldn't happen,
      (setq file ultex-tree-file)		; but just in case
      (ding))
     ((= 1 (length ultex-current-tree-files))
      (setq file (car ultex-current-tree-files)))
     (t
      (setq file 
	    (completing-read "Enter new command in which file: "
			     (mapcar 'list ultex-current-tree-files)
			     nil t nil))))
    ;; open tree file
    (save-window-excursion
      (if (setq treebuf (find-buffer-visiting file))
	  (set-buffer treebuf)
	(find-file file))
      ;; see if completion is already in tree-file
      (goto-char (point-min))
      (setq temp (read (current-buffer)))
      (if (assoc s temp)
	  (progn
	    (message "%s already contains this completion." file)
	    (if treebuf (bury-buffer (current-buffer))
	      (kill-buffer (current-buffer))))
	(goto-char (point-max))
	(re-search-backward "\n)")
	(print c (current-buffer))
	(save-buffer)
	(goto-char (point-min))
	(setq temp (read (current-buffer)))
	(if treebuf (bury-buffer (current-buffer))
	  (kill-buffer (current-buffer)))
	(setq ultex-permanent-alist temp)))))

(defun ultex-delete-csname (str)
  "Delete the string STR from the current alist of TeX control
sequence names.

This has no effect on the files of TeX commands.  Use the command
`ultex-delete-csname-from-tree-file' to delete commands permanently."
  (interactive
   (list (let ((name
		(completing-read
		 "Delete TeX name: " 
		 (mapcar 'list
			 (mapcar 'car
				 (append ultex-permanent-alist
					 ultex-current-commands
					 ultex-current-labels
					 ultex-current-bibs)))
		 nil t nil)))
	   (if name (intern name)))))
  (let ((x (assoc str (append ultex-permanent-alist
			      ultex-current-commands
			      ultex-current-labels
			      ultex-current-bibs))))
    (if x
	(progn
	  (setq ultex-permanent-alist
		(delq x ultex-permanent-alist))
	  (setq ultex-current-commands
		(delq x ultex-current-commands))
	  (setq ultex-current-labels
		(delq x ultex-current-labels))
	  (setq ultex-current-bibs
		(delq x ultex-current-bibs))
	  (message "%s deleted." str))
      (message "%s not found." str))))

(defun ultex-delete-csname-from-tree-file (str)
  "Delete the string STR from the permanent alist of TeX control
sequence names, and erase it from the appropriate tree file.
Prompt for file name if STR appears in more than one."
  (interactive
   (list (completing-read
	  "Delete TeX name: " 
	  (mapcar 'list
		  (mapcar 'car
			  ultex-permanent-alist))
	  nil t nil)))
  (let ((x (assoc str ultex-permanent-alist))
	(tree-files ultex-current-tree-files)
	file
	buf
	alist
	file-list)
    (if x
	(progn
	  ;; get list of all possible files
	  (while tree-files
	    (setq file (car tree-files)
		  tree-files (cdr tree-files))
	    (save-window-excursion
	      (if (file-readable-p file)
		  (progn
		    (setq buf (set-buffer (generate-new-buffer file)))
		    (insert-file-contents file t)
		    (setq alist (read buf))
		    (kill-buffer (current-buffer)))
		(setq alist nil)))
	    (if (assoc str alist)
		(setq file-list (cons file file-list))))
	  (cond
	   ((zerop (length file-list))  	; shouldn't happen,
	    (message "%s not found." str))	; but just in case
	   ((= 1 (length file-list))
	    (ultex-delete-x-from-file x (car file-list))
	    (setq ultex-permanent-alist
		  (delq x ultex-permanent-alist))
	    (message "Command %s deleted from the file %s."
		     str
		     (car file-list)))
	   (t
	    (setq file 
		  (completing-read (concat "Delete "
					   str
					   " from which file: ")
				   (mapcar 'list file-list)
				   nil t nil))
	    (ultex-delete-x-from-file x file)
	    (setq ultex-permanent-alist
		  (delq x ultex-permanent-alist))
	    (message "Command %s deleted from file %s." str file))))
      (message "%s not found." str))))

(defun ultex-delete-x-from-file (x file)
  "Delete TeX command X from tree file FILE.  X should be a list of
the form (string integer) or (string integer . hook)."
  (let (treebuf
	(str (regexp-quote (prin1-to-string x))))
    (save-window-excursion
      (if (setq treebuf (find-buffer-visiting file))
	  (set-buffer treebuf)
	(find-file file))
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "\n?" str "\n?")
	      nil t)
	(replace-match "" nil nil nil nil))
      (save-buffer)
      (if treebuf (bury-buffer (current-buffer))
	(kill-buffer (current-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the various alists of tex commands.
;;

(defun ultex-reset-current-alist ()
  "Reset the current alist to be the permanent alist."
  (interactive)
  (setq ultex-current-commands nil
	ultex-current-labels nil
	ultex-current-bibs nil)
  (message "TeX \\csname alist reset."))

(defun ultex-reset-perm-alist-old ()
  "Reload (by side-effect) permanent alist from ultex-tree-file."
  (let (alist buf)
    (save-window-excursion
      (setq buf (set-buffer (generate-new-buffer ultex-tree-file)))
      (insert-file-contents ultex-tree-file t)
      (setq alist (read buf))
      (kill-buffer (current-buffer))
      (rplaca ultex-permanent-alist (car alist))
      (rplacd ultex-permanent-alist (cdr alist))
      nil)))

(defvar ultex-also-load nil
  "Also load this file of commands.  Intended to be used as a local variable.")

(make-variable-buffer-local 'ultex-also-load)

(defun ultex-reset-perm-alist ()
  "Reload permanent alist.  If any classes, styles, or packages listed
in ultex-extra-tree-files are present, load the corresponding files.
If any associated flag is non-nil, or of no classes (etc.) are
present, then load ultex-tree-file."
  (let (alist
	buf
	(file-list (if ultex-also-load
		       (list ultex-also-load)))
	(load-tree-flag t)
	temp-flag
	packages
	command-list
	file
	junk
	idx
	(ultex-start (save-excursion
		       (ultex-re-search-forward
			"\\\\begin{document}" 
			nil t))))
    (if ultex-extra-tree-files
	(save-excursion
	  (while (ultex-re-search-forward
		  "\\(\\\\documentclass\\|\\\\documentstyle\\|\\\\usepackage\\)\\(\\[[^]]*\\]\\)?{\\([^}]*\\)}"
		  ultex-start t)
	    (setq packages
		  (split-string
		   (buffer-substring-no-properties
		    (match-beginning 3)
		    (match-end 3))
		   ","))
	    (while packages
	      (setq junk (assoc (car packages)
				ultex-extra-tree-files)
		    packages (cdr packages))
	      (if junk
		  ;; if junk is non-nil, set load-tree-flag to be the 'or'
		  ;; of all the flags for the packages.  so erase
		  ;; load-flag and keep track of others with temp-flag.
		  (setq load-tree-flag nil
			file-list (cons (nth 1 junk) file-list)
			temp-flag (or temp-flag
				      (nth 2 junk))))))))
    (setq load-tree-flag (or load-tree-flag temp-flag))
    (if load-tree-flag
	(setq file-list (cons ultex-tree-file file-list)))
    (setq ultex-current-tree-files file-list)
    (while file-list
      (save-window-excursion
	(setq file (car file-list)
	      file-list (cdr file-list))
	(if (file-readable-p file)
	    (progn
	      (setq buf (set-buffer (generate-new-buffer file)))
	      (insert-file-contents file t)
	      (setq alist (read buf))
	      (kill-buffer (current-buffer))
	      (setq command-list (append command-list alist))))))
    (setq ultex-permanent-alist command-list)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the ultra-tex and greek keymaps.
;;

(defvar ultra-tex-menu
  (append '("Ultra-TeX"
	    ("Rescan:"
	     ["Grab commands" ultex-grab-defs t]
	     ["Grab bib info" ultex-grab-bibs t])
	    ("Commands and tree files:"
	     ["Add command to file" ultex-new-cs t]
	     ["Delete command from file" ultex-delete-csname-from-tree-file t])
	    ("Greek keyboards:"
	     ["Show plain keyboard" ultex-show-plain t]
	     ["Show shifted keyboard" ultex-show-shift t]
	     ["Show control keyboard" ultex-show-control t]
	     ["Show meta keyboard" ultex-show-meta t]
	     ["Show meta-shift keyboard" ultex-show-meta-shift t]
	     ["Show meta-control keyboard" ultex-show-meta-control t]))
	  (if (and ultex-picture-type-internal
		   (locate-library "latex-symbols"))
	      '(("Tables of symbols by type:"
		 ["Greek letters" latex-symbol-greek t]
		 ["Operators" latex-symbol-operators t]
		 ["Relations" latex-symbol-relations t]
		 ["Arrows" latex-symbol-arrows t]
		 ["Miscellany" latex-symbol-miscellany t]
		 ["All" latex-symbol-all t])
		("Tables of symbols by package:"
		 ["Plain" latex-symbol-plain t]
		 ["latexsym" latex-symbol-latexsym t]
		 ["amsfonts" latex-symbol-amsfonts t]
		 ["amsmath" latex-symbol-amsmath t]
		 ["amssymb" latex-symbol-amssymb t]
		 ["amsthm" latex-symbol-amsthm t]
		 ["stmaryrd" latex-symbol-stmaryrd t])
		["Look-up symbol" ls-lookup-symbol t])))
  "Menu for Ultra-TeX mode")

(defun ultex-define-ultra-tex-mode-map ()
  "Define ultra-tex-mode-map."
  (setq ultra-tex-mode-map (make-keymap))
  (define-key ultra-tex-mode-map "\"" 'ultex-insert-quote)
  (define-key ultra-tex-mode-map "\\" 'ultex-fast-cs)
  (define-key ultra-tex-mode-map "\C-\\" 'ultex-fast-noslash)
  (define-key ultra-tex-mode-map "\C-m" 'ultex-over-and-out)
  (define-key ultra-tex-mode-map "\C-[\C-m" 'newline)
  (define-key ultra-tex-mode-map "\C-j" 'ultex-par)
  (if ultex-match-braces 
      (define-key ultra-tex-mode-map "{" 'ultex-insert-braces))
  (if ultex-match-brackets
      (define-key ultra-tex-mode-map "[" 'ultex-insert-brackets))
  (if ultex-match-parens  
      (define-key ultra-tex-mode-map "(" 'insert-parentheses))
  (define-key ultra-tex-mode-map "$" 'ultex-smart-dollars)
  (define-key ultra-tex-mode-map "^" 'ultex-superscript)
  (define-key ultra-tex-mode-map "." 'ultex-maybe-dots)
  (define-key ultra-tex-mode-map "_" 'ultex-subscript)
  (define-key ultra-tex-mode-map "\C-i" 'ultex-tab-resume)
  (define-key ultra-tex-mode-map "\C-\\" 'ultex-tab-resume)
  (define-key ultra-tex-mode-map "\C-cg" 'ultex-grab-defs)
  (define-key ultra-tex-mode-map "\C-c\C-r" 'ultex-reset-current-alist)
  (define-key ultra-tex-mode-map "\C-c\C-s" 'get-latex-skel)
  (define-key ultra-tex-mode-map "\C-c\C-n" 'ultex-new-cs)
  (define-key ultra-tex-mode-map "\C-c\C-d" 'ultex-delete-csname)
  (define-key ultra-tex-mode-map "\C-[A"
    '(lambda nil (interactive) (ultex-font "cal")))
  (define-key ultra-tex-mode-map "\C-[B"
    '(lambda nil (interactive) (ultex-font "bf")))
  (define-key ultra-tex-mode-map "\C-[C"
    '(lambda nil (interactive) (ultex-font "sc")))
  (define-key ultra-tex-mode-map "\C-[E"
    '(lambda nil (interactive) (ultex-font "em")))
  (define-key ultra-tex-mode-map "\C-[F"
    '(lambda nil (interactive) (ultex-font "sf")))
  (define-key ultra-tex-mode-map "\C-[I"
    '(lambda nil (interactive) (ultex-font "it")))
  (define-key ultra-tex-mode-map "\C-[M"
    '(lambda nil (interactive) (ultex-font "mit")))
  (define-key ultra-tex-mode-map "\C-[R"
    '(lambda nil (interactive) (ultex-font "rm")))
  (define-key ultra-tex-mode-map "\C-[S"
    '(lambda nil (interactive) (ultex-font "sl")))
  (define-key ultra-tex-mode-map "\C-[T"
    '(lambda nil (interactive) (ultex-font "tt")))
  (define-key ultra-tex-mode-map "\C-[X"
    '(lambda nil (interactive) (ultex-font "text")))
  (define-key ultra-tex-mode-map "\C-[Z"
    '(lambda nil (interactive) (ultex-font "zz")))
  (define-key ultra-tex-mode-map "\C-[U"
    '(lambda nil (interactive) (ultex-font "up")))
  (if ultex-dont-use-completion
      (progn
	(define-key ultra-tex-mode-map "\C-i" 'ultex-try-to-complete)
	(define-key ultra-tex-mode-map "\\" 'self-insert-command)
	(define-key ultra-tex-mode-map "\C-\\" nil)))
  (define-key ultra-tex-mode-map "`" 'ultex-greek-map)
  (easy-menu-define ultra-tex-menu
		    ultra-tex-mode-map
		    "Menu bar for Ultra-TeX mode"
		    ultra-tex-menu))

(defun ultex-reset-greek-keys ()
  "Reload Greek key definitions from ultex-greek-keys-file."
  (interactive)
  (define-prefix-command 'ultex-greek-map)
  (define-prefix-command 'ultex-greek-meta-map)
  (suppress-keymap ultex-greek-map 'no-digits)
  (suppress-keymap ultex-greek-meta-map 'no-digits)
  (define-key ultex-greek-map "\\" 'ultex-show-plain)
  (define-key ultex-greek-map "|"  'ultex-show-shift)
  (define-key ultex-greek-map '[(control ?\\)] 'ultex-show-control)
  (define-key ultex-greek-meta-map '[(control ?\\)] 'ultex-show-meta-control)
  (define-key ultex-greek-meta-map "\\" 'ultex-show-meta)
  (define-key ultex-greek-map '[(meta ?\\)] 'ultex-show-meta)
  (define-key ultex-greek-meta-map "|" 'ultex-show-meta-shift)
  (define-key ultex-greek-map '[(meta ?|)] 'ultex-show-meta-shift)
  (define-key ultex-greek-meta-map '[(control ?\\)] 'ultex-show-meta-control)
  (define-key ultex-greek-map '[(meta control ?\\)] 'ultex-show-meta-control)
  (let (buf line map key def done err)
    (save-window-excursion
      (message "Reading Greek keys file")
      (if (file-readable-p ultex-greek-keys-file)
	  (progn
	    (setq buf (set-buffer (generate-new-buffer
				   ultex-greek-keys-file)))
	    (insert-file-contents ultex-greek-keys-file t)
	    (goto-char (point-min))
	    (while (not done)
	      (condition-case err
		  (progn
		    (setq line (read buf))
		    (if (and (= (length line) 4)
			     (eq (car line) 'define-key))
			(progn
			  (setq map (cadr line)
				key (caddr line)
				def (cadddr line))
			  (if (and (listp key)
			      (eq (car key) 'quote))
			      (setq key (cadr key)))
			  (if (stringp def)
				(define-key map key
				  `(lambda nil
				     "Insert Greek key string"
				     (interactive)
				     (ultex-greek-insert ,def)))
			    (define-key map key def)))))
		;; clean up error messages depending on version of emacs??
		((invalid-read-syntax
		  wrong-type-argument)
		 (message "Greek key %s not defined: %s"
			  key err))
		((error
		  end-of-file)
		 (or (equal err '(end-of-file))
		     (equal (car err) 'end-of-file)
		     (message "Greek keys error: %s" err))
		 (setq done t))))
	    (set-buffer-modified-p nil)
	    (kill-buffer (current-buffer))))
      (message "Done reading Greek keys"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grab commands, labels, bibliography data.
;;

(defun ultex-grab-defs (&optional fn no-commands no-labels)
  "Get list of commands and labels from optional arg FILE, or from
current buffer if no argument is given.

Read defs recursively from input and include files.  Optional args:
NO-COMMANDS, NO-LABELS.  If either is non-nil, don't grab commands
and/or labels."
  (interactive "P")
  (and fn (not (stringp fn))
       (setq fn (expand-file-name
		 (read-file-name
		  "Grab TeX commands from file: "))))
  (if (null fn)
      (save-excursion
	(message "Grabbing from current-buffer")
	(ultex-grab nil (not no-commands) (not no-labels))
	(ultex-grab-loop (ultex-grab-inputnames nil)
			 (not no-commands)
			 (not no-labels))
	(message "Grabbing done"))
    (ultex-grab-loop (list fn) (not no-commands) (not no-labels))))

(defun ultex-grab-bibs (&optional fn)
  "Get list of commands and labels from optional arg FILE, or from
current buffer if no argument is given.

Read defs recursively from input and include files.  Optional args:
NO-COMMANDS, NO-LABELS.  If either is non-nil, don't grab commands
and/or labels."
  (interactive "P")
  (and fn (not (stringp fn))
       (setq fn (expand-file-name
		 (read-file-name
		  "Grab Bibliographical information from file: "))))
  (if (null fn)
      (save-excursion
	(message "Grabbing bibliographical information from current-buffer")
	(ultex-grab-bibs-internal nil)
	(ultex-grab-loop (ultex-grab-inputnames nil)
			 nil nil 'bibs)
	(message "Grabbing bibliographical information done"))
    (ultex-grab-loop (list fn) nil nil 'bibs)))

(defun ultex-grab-loop (lst commands labels &optional bibs)
  "Grab defs from each file in LST.  Args COMMANDS and LABELS specify
what to grab--they are passed as args to ultex-grab.  Optional arg
BIBS: if non-nil, grab bibliographical references."
  (let ((pos 0) fn)
    (while (setq fn (nth pos lst))
      (condition-case nil
	  (progn
	    (save-excursion
	      (set-buffer
	       (generate-new-buffer fn))
	      (insert-file-contents fn t)
	      (setq default-directory
		    (file-name-directory fn))
	      (setq lst (ultex-grab-inputnames lst))
	      (kill-buffer (current-buffer)))
	    (if (or commands labels)
		(progn
		  (message "Grabbing from %s" fn)
		  (ultex-grab fn commands labels)))
	    (if bibs
		(ultex-grab-bibs-internal fn)))
	(file-error (message "Grabbing from %s ... Error opening file" fn)))
      (setq pos (1+ pos)))))

(defun ultex-grab-inputnames (lst)
  "Build list of input and include files from which to grab defs.
Argument LST: if input/include file is already in LST, then don't
add it.  This searches for lines *beginning* `\input' or `\include', so
to prevent a particular file from being added to the list, start that
line with a space or tab.  It also uses the value of the variable
TeX-master as one of the 'input' files."
  (let (fn
	ext
	temp-dirs
	dir
	exists-p
	(dirs (ultex-parse-colon-path (getenv "TEXINPUTS"))))
    (setq dirs (cons (file-name-as-directory (expand-file-name ".")) dirs))
    (if (and (boundp 'TeX-master) (stringp TeX-master))
	(progn
	  (setq fn (expand-file-name TeX-master)
		ext (substring fn (- (min 4 (length fn))) nil))
	  (if (file-exists-p fn)
	      (progn
		(if (not (or (string= ext ".tex") (string= ext ".sty")))
		    (setq fn (concat fn ".tex")))
		(or (member fn lst) (setq lst (append lst (list
							 fn))))))))
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\\\in\\(put\\|clude\\)\\(\\s-\\|{\\)+\\([^ }]+\\)[ }]"
	    nil t)
      (setq fn (buffer-substring (match-beginning 3)
				 (match-end 3))
		ext (substring fn (- (min 4 (length fn))))
	    exists-p nil
	    temp-dirs dirs)
      (if (not (or (string= ext ".tex") (string= ext ".sty")))
	  (setq fn (concat fn ".tex")))
      (while (and (null exists-p) temp-dirs)
	(setq dir (car temp-dirs)
	      exists-p (file-readable-p (concat dir fn))
	      temp-dirs (cdr temp-dirs)))
      (if (null exists-p) (message "Grabbing from %s ... Error opening file" fn))
      (or (null exists-p) (member fn lst)
	  (setq lst (append lst (list (concat dir fn)))))))
  lst)

(defun ultex-grab (&optional fn commands labels min max)
  "Scan current buffer for TeX commands and/or labels.

Optional arguments: FILE: if non-nil, scan FILE instead of current
buffer.  COMMANDS: if non-nil, grab TeX commands.  LABELS: if non-nil,
grab labels.  MIN, MAX: if non-nil, bound scan by these file
positions.

If scanning for commands, searches for \\(re)newcommand,
\\providecommand, \\DeclareMathOperator, \\DeclareMathSymbol, \\def,
\\(re)newenvironment and \\(re)newtheorem and parses the buffer
accordingly.  If scanning for labels, searches for \\label.

In either case, if the buffer is read only then ask user if we should
really grab the defs.  If the buffer contains the string
--ultex--dont--grab-- near the top, then don't grab the defs."  
  (interactive "P")
  (and fn (not (stringp fn))
       (setq fn (expand-file-name
		 (read-file-name
		  "Grab TeX commands from file: "))))
  (or min (setq min (point-min)))
  (or max (setq max (point-max)))
  (let (command-list label-list csname nargs ultex-end ultex-start no-star)
    (save-excursion
      (save-window-excursion
	(and fn
	     (progn
	       (set-buffer
		(generate-new-buffer fn))
	       (insert-file-contents fn t)))
	(setq ultex-end (save-excursion
			  (ultex-re-search-forward
			   "\\\\end{document}" 
			   max t))
	      ultex-start (save-excursion
			    (ultex-re-search-forward
			     "\\\\begin{document}" 
			     max t)))
	(or ultex-start (setq ultex-start max))
	(or ultex-end (setq ultex-end max))
	(if (or (search-forward "--ultex--dont--grab--" ultex-start t)
		(and buffer-read-only
		     (or ultex-alist-local-flag
			 (not (y-or-n-p
			       (format  "Grab TeX commands from buffer %s? "
					(buffer-name (current-buffer))))))))
	    (progn (message "Not grabbing defs from this buffer.")
		   nil)
	  (if commands
	      (progn
		(goto-char min)
		(while (setq csname
			     (ultex-get-next
			      "newtheorem" 3
			      "\\\\\\(re\\)?newtheorem\\(\\*?\\)\\s-*{\\([a-zA-Z]+\\)\\s-*}"
			      ultex-start 3))
		  (setq no-star (= (match-beginning 2)
				   (match-end 2)))
		  (setq command-list
			(cons (list
			       (concat "\\begin{" csname "}"
				       (if no-star
					   "\\label{}\n\n\\end{"
					 "\n\n\\end{")
				       csname "}\n")
			       (- (+ 10
				     (if no-star 0 -2)
				     (length csname))))
			      command-list)))
		(goto-char min)
		(while (setq csname
			     (ultex-get-next
			      "newenvironment" 3
			      "\\\\\\(re\\)?newenvironment\\s-*{\\([a-zA-Z]+\\)\\s-*}"
			      ultex-start 2))
		  (setq nargs (or (and (looking-at "\\[\\([1-9]\\)\\]")
				       (string-to-int
					(buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1))))
				  0)
			command-list 
			(cons (list
			       (concat "\\begin{" csname "}"
				       (apply 'concat (make-list nargs "{}"))
				       "\\label{}\n\n\\end{" csname "}\n")
			       (+ (- (length csname)) -10 
				  (if (= 0 nargs) 0
				    (+ 6 (* -2 nargs)))))
			      command-list)))
		(goto-char min)
		(while (setq csname
			     (ultex-get-next
			      "\\DeclareMath" 0
			      "\\\\DeclareMath\\(Operator\\|Symbol\\)[*]?\\s-*[{]?\\(\\\\[a-zA-Z]+\\s-*\\)[{}]"
			      ultex-start 2))
		  (setq command-list (cons (list (concat csname " ") 0)
					   command-list)))
		(goto-char min)
		(while (setq csname
			     (ultex-get-next
			      "command" 8
			      "\\\\\\(\\(re\\)?new\\|provide\\)command\\s-*{?\\(\\\\[a-zA-Z]+\\|\\\\.\\)\\s-*}?"
			      ultex-end 3))
		  (setq nargs (or (and (looking-at "\\[\\([1-9]\\)\\]")
				       (string-to-int
					(buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1))))
				  0)
			command-list
			(cons (list (concat
				     csname
				     (apply 'concat (make-list nargs "{}"))
				     (if (and (zerop nargs)
					      ultex-append-space-to-defs)
					 " " ""))
				    (if (zerop nargs) 0 (1+ (* -2 nargs))))
			      command-list)))
		(goto-char min)
		(while (setq csname
			     (ultex-get-next
			      "\\def" 0
			      "\\\\def\\s-*\\(\\\\[a-zA-Z]+\\|\\\\.\\)\\([^{]*\\){"
			      ultex-end 1))
		  (let* ((arg-temp (buffer-substring-no-properties
				    (match-beginning 2)
				    (match-end 2)))
			 (i 0)
			 (l (1- (length arg-temp))))
		    (string-match "\\s-*" arg-temp)
		    (if (/= (match-end 0) (1+ l))
			nil
		      (setq arg-temp ""
			    l -1))
		    (while (< i l)
		      (if (/= (aref arg-temp i) ?#) nil
			(aset arg-temp i ?{) (aset arg-temp (1+ i) ?}))
		      (setq i (1+ i)))
		    (if (= l -1) (setq csname (concat csname " ")))
		    (setq i (if (setq i (string-match "{}" arg-temp))
				(- i l)
			      0))
		    (setq command-list (cons
					(list (concat csname arg-temp) i)
					command-list))))))
	  (if labels
	      (progn
		(goto-char min)
		(while (setq csname
			     (ultex-get-next "\\label" 0
					     "\\\\label{\\([^}]+\\)}"
					     ultex-end 1 t))
		  (setq label-list
			(append (list 
				 (list (concat "\\ref{" csname "} ") 0)
				 (list (concat "\\eqref{" csname "} ") 0)
				 (list (concat "\\pageref{" csname "} ") 0))
				label-list)))))
	  (and fn (kill-buffer (current-buffer))))))
    (while command-list
      (ultex-add-to-list (car command-list) 'commands)
      (setq command-list (cdr command-list)))
    (while label-list
      (ultex-add-to-list (car label-list) 'labels)
      (setq label-list (cdr label-list)))))

;; things seem to go faster with the (search-forward) at the start...
(defun ultex-get-next (string back regexp limit match
			      &optional allow-space)
  "Arguments: STRING, BACK, REGEXP, LIMIT, MATCH.  Works like
  (re-search-forward REGEXP LIMIT t)
  (buffer-substring (match-beginning MATCH) (match-end MATCH))
Uses STRING to speed things up a bit--search for STRING first, then go
back (length STRING) + BACK characters.  It should then be faster to
find REGEXP, even if the regular expression is quite complicated.

Optional arg: ALLOW-SPACE.  If non-nil, ignore value of
ultex-no-grab-if-space.  Intended for use when grabbing labels."
  (let ((pos (point)))
    (search-forward string limit 'move)
    (or (ultex-forward-char (* -1 (+ (length string) back)))
	(goto-char pos))
    (and (ultex-re-search-forward
	  (if (or allow-space (not ultex-no-grab-if-space))
	      regexp (concat "^" regexp))
	  limit t)
	 (buffer-substring-no-properties (match-beginning match)
					 (match-end match)))))

(defun ultex-grab-bibs-internal (&optional fn)
  "Insert in the TeX completion alist templates for all new \\bibitem's
in current buffer, in associated bbl and bib files, or with arg,
from FILE.  If the buffer is read only then ask user if we should
really grab the defs.  If the buffer contains the string
--ultex--dont--grab-- near the top, then don't grab the defs."
  (interactive "P")
  (and fn (not (stringp fn))
       (setq fn (expand-file-name
		 (read-file-name
		  "Grab bibliographical entries from file: "))))
  (let (bib-list ultex-end file args keep-path path)
    (save-excursion
      (save-window-excursion
	(and fn
	     (progn
	       (set-buffer
		(generate-new-buffer fn))
	       (insert-file-contents fn t)))
	(goto-char (point-min))
	(if (search-forward "--ultex--dont--grab--"  3000 t)
	    nil
	  (goto-char (point-min))
	  (if (and buffer-read-only
		   (or ultex-alist-local-flag
		       (not (y-or-n-p
			     (format  "Grab bibliographical entries from buffer %s? "
				      (buffer-name (current-buffer)))))))
	      (progn (message "Not grabbing bibs from this buffer.")
		     nil)
	    (goto-char (point-min))
	    (setq bib-list (append (ultex-scan-bbl) bib-list))
	    (setq file (buffer-file-name (current-buffer)))
	    (if (and file (stringp file))
		(progn
		  (setq file (if (string=
				  (substring file -4) ".tex")
				 (substring file 0 -4)
			       file))
		  (if (file-exists-p (concat file ".bbl"))
		      (setq bib-list
			    (append
			     (ultex-scan-bbl (concat file ".bbl"))
			     bib-list)))))
	    (setq ultex-end (save-excursion
			      (ultex-re-search-forward
			       "\\\\end{document}" 
			       nil t))
		  file nil)
	    (search-forward "\\bibliography{" ultex-end 'move)
	    (beginning-of-line nil)
	    (while (ultex-re-search-forward
		    "\\\\bibliography{\\([^}]+\\)}"
		    ultex-end t)
	      (setq args (buffer-substring-no-properties
			  (match-beginning 1)
			  (match-end 1))
		    keep-path (ultex-parse-colon-path
			       (getenv "BIBINPUTS")))
	      (while (< 0 (length args))
		(if (string-match ",[ \t\n]*" args)
		    (setq file (substring args 0 (match-beginning 0))
			  args (substring args (match-end 0)))
		  (setq file args
			args ""))
		(setq path keep-path)
		(while path
		  (if (file-exists-p (concat (car path) file ".bib"))
		      (setq bib-list
			    (append
			     (ultex-scan-bib
			      (concat (car path) file ".bib"))
			     bib-list)
			    path nil)
		    (setq path (cdr path))))))
	    (search-forward "\\bibliography{" ultex-end 'move)
	    (or (eobp) (beginning-of-line nil))))
	(and fn (kill-buffer (current-buffer)))))
    (while bib-list
      (ultex-add-to-list (car bib-list) 'bibs)
      (setq bib-list (cdr bib-list)))))

(defun ultex-scan-bbl (&optional fn)
  "Scan current buffer, or with optional arg, FILE, for bibitems.
Return list of \\cite and \\nocite commands."
  (let (new-list
	pos
	citation)
    (save-excursion
      (save-window-excursion
	(if fn
	    (progn
	      (set-buffer
	       (generate-new-buffer fn))
	      (insert-file-contents fn t)
	      (message "Scanning %s for \\bibitems" fn)))
	(goto-char (point-min))
	(ultex-re-search-forward "\\\\begin{thebibliography}" nil 2)
	(setq pos (save-excursion
		    (ultex-re-search-forward
		     "\\\\end{thebibliography}" nil t)))
	(while (ultex-re-search-forward
		"\\\\bibitem\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}"
		pos t)
	  (setq citation (buffer-substring-no-properties
			  (match-beginning 2)
			  (match-end 2))
		new-list (cons (list (concat "\\cite[]{" citation "}")
				     (- (+ 3 (length citation))))
			       (cons (list (concat "\\nocite[]{" citation"}")
					   (- (+ 3 (length
						    citation))))
				     new-list)))
	  (search-forward "\\bibitem" pos 'move)
	  (or (eobp)
	      (beginning-of-line nil)))
	(and fn (kill-buffer (current-buffer)))))
    (if fn (message "Scanning done"))
    new-list))

(defun ultex-scan-bib (&optional fn)
  "Scan current buffer, or with optional arg, FILE, for @article,
@book, etc. entries.  Return list of \\cite and \\nocite."
  (let (new-list citation)
    (save-excursion
      (save-window-excursion
	(if fn
	     (progn
	       (set-buffer
		(generate-new-buffer fn))
	       (insert-file-contents fn t)
	       (message "Scanning %s for bibliographical data" fn))
	  (message "Scanning current buffer for bibliographical data"))
	(goto-char (point-min))
	(while (re-search-forward "@\\([^{]+\\){\\([^,}]+\\)," nil t)
	  (setq citation (buffer-substring-no-properties
			  (match-beginning 2)
			  (match-end 2))
		new-list (cons (list (concat "\\cite[]{" citation "}")
				     (- (+ 3 (length citation))))
			       (cons (list (concat "\\nocite[]{"
						   citation"}")
					   (- (+ 3 (length
						    citation))))
				     new-list)))
	  (search-forward "@" nil 'move)
	  (or (eobp)
	      (beginning-of-line nil)))
	(and fn (kill-buffer (current-buffer)))))
    (message "Scanning done")
    new-list))

(defun ultex-add-def (def)
  "Push (<csname> <point-adjust> . <hook>) cons DEF on the current
alist if it isn't there already.  If already there, return def from
alist, else return nil."
  (or (assoc (car def) ultex-current-commands)
      (prog1 nil
	(setq ultex-current-commands
	      (cons def ultex-current-commands)))))

(defun ultex-add-to-list (def arg)
  "Push (<csname> <point-adjust> . <hook>) cons DEF on to appropriate
alist if it isn't there already.  If already there, return def, else
return nil.  Appropriate alist is determined by second argument, ARG:
allowable values are 'commands, 'labels, or 'bibs, in which case alist
is ultex-current-commands, -labels, or -bibs."
  (cond
   ((eq arg 'commands) 
    (or (assoc (car def) ultex-current-commands)
	(prog1 nil
	  (setq ultex-current-commands
		(cons def ultex-current-commands)))))
   ((eq arg 'labels)
    (or (assoc (car def) ultex-current-labels)
	(prog1 nil
	  (setq ultex-current-labels
		(cons def ultex-current-labels)))))
   ((eq arg 'bibs)
    (or (assoc (car def) ultex-current-bibs)
	(prog1 nil
	  (setq ultex-current-bibs
		(cons def ultex-current-bibs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check styles 
;;

(defun ultex-check-styles nil
  "See what class, style, packages are used, and respond accordingly.
At the moment, this just sets ultex-dots-behavior to 'ams' if the
variable ultex-dots-check-styles is non-nil and an ams
class/style/package--i.e., something whose name starts with
\"ams\"---is being used."
  (let (done
	packages
	(ultex-start (save-excursion
		       (ultex-re-search-forward
			"\\\\begin{document}" 
			nil t))))
    (if ultex-dots-check-styles
	(save-excursion
	  (while (and (not done)
		     (ultex-re-search-forward
		      "\\(\\\\documentclass\\|\\\\documentstyle\\|\\\\usepackage\\)\\(\\[[^]]*\\]\\)?{\\([^}]*\\)}"
		      ultex-start t))
	    (setq packages (buffer-substring-no-properties
			    (match-beginning 3)
			    (match-end 3)))
	    (if (string-match "\\(\\`\\|,\\)ams" packages)
		(setq ultex-dots-behavior 'ams
		      done t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intelligent RET key.
;;

(defconst ultex-endings " ?\\\\]
?\\| ?\\$\\$
\\|
?\\\\end{[^}]*}
?\\|\\$\\|\\]\\|[a-z]*}\\(
?\\\\label\\)?\\| ?{\\| ?\\\\}\\| ?\\\\)")

(defun ultex-forward-char (&optional n)
  "Move point right N characters (left if N is negative).  Return t if
successful--i.e., you don't hit the beginning or end of the
buffer--and return nil otherwise."
  (interactive "p")
  (or n (setq n 1))
  (let ((pt (point)))
    (not (condition-case nil
	     (forward-char n)
	   (error (goto-char pt)
		  t)))))

(defun ultex-over-and-out nil
  "Go up one level of \\], \\end{...}, $, ], or }; additionally skip
any immediately following {.  Otherwise, move forward cleverly.
Delete empty [] and \\label{}."
  (interactive)
  (ultex-make-ref-from-label)
  (ultex-ret-add-command)
  (if ultex-subscript-delete-trailing-space
      (save-excursion
	(ultex-forward-char -1)
	(if (looking-at " ")
	    (delete-char 1))))
  (or (and (not (bobp))
	   (save-excursion
	     (if (ultex-forward-char -6)
		 (looking-at "\\\\cite\\[\\]")))
	   (progn (delete-char -1) (delete-char 1)
		  (search-forward "{" nil t)
		  (or (looking-at "}")
		      (search-forward "}" nil t))))
      (and (not (bobp))
	   (looking-at "[^][]*\\]{")
	   (save-excursion
	     (and (search-backward "["
				   (save-excursion
				     (goto-char (point-min))
				     (ultex-forward-char 5)
				     (point)) t)
		  (and (ultex-forward-char -5)
		       (looking-at "\\\\cite\\["))))
	   (search-forward "{" nil t)
	   (or (looking-at "}")
	       (search-forward "}" nil t)))
      (and (not (bobp))
	   (save-excursion (and (ultex-forward-char -1)
				(looking-at "\\[\\]")))
	   (progn (delete-char -1) (delete-char 1)
		  (skip-chars-forward "[") (skip-chars-forward "{")
		  t))
      (and (not (bobp))
	   (save-excursion (and (ultex-forward-char -7)
				(looking-at "\\\\label{}")))
	   (progn (delete-char -7) (delete-char (if (bolp) 2 1))
		  (skip-chars-forward "[") (skip-chars-forward "{")
		  t))
      (if (looking-at ultex-endings)
	  (progn (goto-char (match-end 0))
		 (skip-chars-forward "[") (skip-chars-forward "{"))
	(if (eolp)
	    (if (looking-at "\n\n")
		(ultex-forward-char 1)
	      (newline))
	  ;;(if (looking-at "\\s-*\\\\?\\w+")
	  (if (looking-at "\\s-*\\(\\s<\\|\\\\?\\w\\)+\\|\\s-+")
	      (goto-char (match-end 0))
	    (ultex-forward-char 1)))))
  (if (and (eolp)
	   (ultex-macro-line-p))
      (progn (next-line 1) (beginning-of-line))))

(defun ultex-make-ref-from-label ()
  "If cursor is at the end of \"\label{blah}\", then add \ref{blah},
\pageref{blah}, and \eqref{blah} to ultex-current-labels."
  (let ((start (point))
	(labelname "")
	end
	(add-label t))
    (save-excursion
      (and (looking-at "}")
	   (condition-case nil
	       (progn
		 (forward-char 1)
		 (forward-sexp -1)
		 (setq end (1+ (point)))
		 (forward-char -6)
		 (looking-at "\\\\label{\\([^}]+\\)}"))
	     (error nil))
	   (progn
	     (setq labelname (buffer-substring-no-properties start end))
	     (if ultex-alist-local-flag
		 (setq add-label
		       (not
			(if (ultex-add-to-list 
			     (list (concat "\\pageref{" labelname "} ") 0)
			     'labels)
			    (message "Label %s has already been defined."
				     labelname)
			  nil))))
	     (if (and add-label ultex-rescan-after-new-label)
		 (progn
		   (message "Adding ref, eqref, and pageref for %s"
			    labelname)
		   (setq ultex-current-labels nil)
		   (ultex-grab-defs nil 'no-commands))
	       (ultex-add-to-list
		(list (concat "\\eqref{" labelname "} ") 0)
		'labels)
	       (ultex-add-to-list
		(list (concat "\\ref{" labelname "} ") 0)
		'labels)))))))

(defun ultex-ret-add-command ()
  "If cursor is at the end of \"\newcommand{\blah}\", etc., then add
\blah to ultex-current-commands.
This just calls ultex-grab, so it should understand: (re)newtheorem,
DeclareMathOperator, (re)newenvironment, (re))newcommand,
DeclareMathSymbol, providecommand, and def."
  (let (start end)
    (if ultex-return-checks-for-new-commands
	(save-excursion
	  (ultex-forward-char 1)
	  (or (looking-at "\\[\\]")
	      (progn
		(setq end (point))
		(beginning-of-line)
		(setq start (point))
		(ultex-grab nil t nil start end)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special predicates.
;;

(defun ultex-macro-line-p nil
  (save-excursion
    (end-of-line)
    (and (save-excursion (beginning-of-line) (looking-at "\\\\"))
	 (save-restriction
	   (save-excursion
	     (narrow-to-region (- (point) (current-column)) (point))
	     (goto-char (point-min))
	     (re-search-forward
	      "^\\(\\\\def\\s-*\\)?\\\\.[a-zA-Z*]*" nil t) ; skip leading cs
	     (let ((c t))
	       (while (not (eobp))
		 (setq c (and c (looking-at "{\\|\\[")))
		 (condition-case nil (forward-sexp 1)
		   (error (end-of-line) (setq c nil))))
	       c))))))

(defun ultex-math-mode (&optional arg)
  "t if point appears to be within math mode, else nil.
If nil, warn user unless given optional ARG."
  (interactive)
  (if (ultex-commented-outp) nil
    (let ((origin (point))
	  (back t)
	  (anybox (concat (or ultex-non-math-cs-regexp "")
			  (if (and ultex-non-math-cs-regexp
				   ultex-math-cs-regexp)
			      "\\|" "")
			  (or ultex-math-cs-regexp "")))
	  (skip (list (point)))
	  bstart c junk)
      (if (save-excursion
	    (and ultex-def-regexp
		 (re-search-backward ultex-def-regexp nil t)
		 (condition-case nil
		     (progn
		       (goto-char (match-end 0))
		       (forward-sexp 1)
		       (> (point) origin))
		   (error
		    t))))
	  'def    
	(save-excursion
	  (while back
	    (re-search-backward (concat "\\`\\|\n\n\\|" ultex-math-start
					"\\|" anybox) nil 'move)
	    (setq bstart (point))
	    (if (or (save-excursion
		      (ultex-forward-char -1)
		      (looking-at "\\\\\\\\\\["))
		    (looking-at anybox))
		(condition-case nil
		    (progn (forward-sexp 2)
			   (if (<= (point) origin)
			       (progn
				 (setq skip (cons bstart (cons (point) skip)))
				 (goto-char bstart))
			     (goto-char bstart)
			     (setq c (and ultex-math-cs-regexp
					  (looking-at ultex-math-cs-regexp))
				   back nil)))
		  (error 
		   (goto-char bstart)
		   (setq c (and ultex-math-cs-regexp
				(looking-at ultex-math-cs-regexp))
			 back nil)))
	      (setq c (and (looking-at ultex-math-start)
			   (not (progn
				  (re-search-forward ultex-math-start origin t)
				  (re-search-forward ultex-math-end 
						     origin 'move))))
		    back nil)
	      (goto-char bstart)))
	  (while skip
	    (if (looking-at "\\$")
		(setq junk "\\`")
	      (setq junk "\\([^\\]\\|\\`\\)"))
	    (if (and (< (point) (car skip))
		     (re-search-forward (concat junk "\\$\\$\\|\\$")
					(car skip) 'move))
		(or (save-excursion
		      (ultex-forward-char -2)
		      (looking-at "\\\\\\$"))
		    (setq c (not c)))
	      (if (cdr skip)
		  (progn (goto-char (car (cdr skip)))
			 (setq skip (cdr (cdr skip))))
		(setq skip nil))))
	  (if (or c arg)  nil
	    (message "You don't appear to be in math mode.") (ding))
	  c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Greek keyboard.
;;

(defun ultex-greek-insert (str)
  "Insert string STR, beep if not in math mode."
  (interactive)
  (ultex-math-mode)
  (insert str)
  (if (> (current-column) fill-column)
      (run-hooks 'auto-fill-hook)))

(defvar ultex-123-row '("" (?1 ?! . nil) (?2 ?@ . ?\C-@) (?3 ?# . nil)
			   (?4 ?$ . nil) (?5 ?% . nil) (?6 ?^ . ?\C-^)
			   (?7 ?& . nil) (?8 ?* . nil) (?9 ?( . nil)
			   (?0 ?) . nil) (?- ?_ . ?\C-_) (?= ?+ . nil)
			   (?` ?~ . nil)))
(defvar ultex-qwe-row '("  " (?q ?Q . ?\C-q) (?w ?W . ?\C-w) (?e ?E . ?\C-e)
			      (?r ?R . ?\C-r) (?t ?T . ?\C-t) (?y ?Y . ?\C-y)
			      (?u ?U . ?\C-u) (?i ?I . ?\C-i) (?o ?O . ?\C-o)
			      (?p ?P . ?\C-p) (?[ ?{ . nil) (?] ?} . ?\C-])))
(defvar ultex-asd-row '("   " (?a ?A . ?\C-a) (?s ?S . ?\C-s) (?d ?D . ?\C-d)
			       (?f ?F . ?\C-f) (?g ?G . ?\C-g) (?h ?H . ?\C-h)
			       (?j ?J . ?\C-j) (?k ?K . ?\C-k) (?l ?L . ?\C-l)
			       (?; ?: . nil) (?' ?" . nil)))
(defvar ultex-zxc-row '("     "
			(?z ?Z . ?\C-z) (?x ?X . ?\C-x) (?c ?C . ?\C-c)
			(?v ?V . ?\C-v) (?b ?B . ?\C-b) (?n ?N . ?\C-n)
			(?m ?M . ?\C-m) (?, ?< . nil) (?. ?> . nil)
			(?/ ?? . ?\C-?) (?  ?  . nil)))

(defun ultex-show-plain () (interactive)
  (ultex-show-keys 'plain))
(defun ultex-show-shift () (interactive)
  (ultex-show-keys 'shift))
(defun ultex-show-control () (interactive)
  (ultex-show-keys 'control))
(defun ultex-show-meta () (interactive)
  (ultex-show-keys 'meta))
(defun ultex-show-meta-shift () (interactive)
  (ultex-show-keys 'meta-shift))
(defun ultex-show-meta-control () (interactive)
  (ultex-show-keys 'meta-control))

(defun ultex-show-keys (arg)
  "Display a Greek shift (`) keyboard in a window.
ARG is 'plain, 'shift, 'control, 'meta, 'meta-shift, or 'meta-control."
  (interactive)
  (let ((bufname (cdr
		  (assoc arg
			 '((plain . " *greek keyboard*")
			   (shift . " *Greek Keyboard*")
			   (control . " *^Greek ^Keyboard*")
			   (meta . " *M-greek M-keyboard*")
			   (meta-shift . " *M-Greek M-Keyboard*")
			   (meta-control . " *M-C-Greek M-C-Keyboard"))))))
    (with-output-to-temp-buffer bufname
      (ultex-show-row ultex-123-row arg bufname)
      (ultex-show-row ultex-qwe-row arg bufname)
      (ultex-show-row ultex-asd-row arg bufname)
      (ultex-show-row ultex-zxc-row arg bufname))
    (let ((ow (selected-window))
	  (w (get-buffer-window (get-buffer bufname)))
	  (default-height)
	  (size 9))
      (if (and ultex-display-pix-internal
	       (featurep ultex-picture-type-internal))
	  (progn
	    (setq default-height (face-height 'default))
	    (cond
	     ((< default-height 9)
	      (setq size (+ size 4)))
	     ((< default-height 11)
	      (setq size (+ size 3)))
	     ((< default-height 14)
	      (setq size (+ size 2)))
	     ((< default-height 15)
	      (setq size (+ size 1)))
	     (t
	      (setq size (+ size 1))))))
      (select-window w)
      (setq truncate-lines t)
      (enlarge-window (- size (window-height w)))
      (select-window ow))))

(defun ultex-show-row (row arg bufname)
  (let ((width (frame-width))
	key-space-before		; space before key names
	key-space-after			; space after key names
	trunc-length			; length at which to truncate str
	key-def                         ; what each key does
	(key-list (cdr row))            ; list of keys in row
	key
	str
	char
	picture-file
	default-width
	(fix-spacing 0)
	(extra-spacing 0))
    (cond ((< width 92)
	   (setq key-space-before "  "
		 key-space-after "  "
		 trunc-length 5))
	  ((< width 104)
	   (setq key-space-before "  "
		 key-space-after "   "
		 trunc-length 6))
	  ((< width 116)
	   (setq key-space-before "   "
		 key-space-after "   "
		 trunc-length 7))
	  (t
	   (setq key-space-before "   "
		 key-space-after "    "
		 trunc-length 8)))
    (princ (car row))
    (if (and ultex-display-pix-internal
	     (featurep ultex-picture-type-internal))
	(setq default-width (face-width 'default)))
    (while key-list
      (setq key (car key-list)
	    key-list (cdr key-list)
	    char (cond ((memq arg '(plain meta control meta-control))
			(car key))
		       ((memq arg '(shift meta-shift))
			(car (cdr key)))
		       (t (cdr (cdr key))))
	    str (and char
		     (lookup-key
		      ultex-greek-map
		      (cond ((eq arg 'plain)
			     (vector (list (car key))))
				 ((eq arg 'shift)
				  (vector (list (cadr key))))
				 ((eq arg 'control)
				  (vector (list 'control (car key))))
				 ((eq arg 'meta)
				  (vector (list 'meta (car key))))
				 ((eq arg 'meta-shift)
				  (vector (list 'meta (cadr key))))
				 ((eq arg 'meta-control)
				  (vector (list 'meta 'control
						(car key)))))))
	    str (and char (listp str) (cadr (car (cddddr str))))
	    key-def (and (stringp str)
			 (if (string= " " (substring str -1))
			     (substring str 1 -1)
			   str))
	    picture-file (and (stringp key-def)
			      (concat ultex-picture-directory
				      (symbol-name
				       ultex-picture-type-internal)
				      "/"
				      key-def
				      "."
				      (symbol-name
				       ultex-picture-type-internal))))
      (if (and ultex-display-pix-internal
	       (stringp picture-file)
	       (featurep ultex-picture-type-internal)
	       (file-exists-p picture-file))
	  (progn
	    (princ (make-string (+ 2 (/ 2 trunc-length)) ?\ ))
	    (setq width (ls-display-picture picture-file
					       0
					       ;; (- trunc-length fix-spacing)
					       bufname)
		  extra-spacing (+ extra-spacing
				   (- width default-width)))
	    (cond
	     ((> extra-spacing 1)
	      (setq fix-spacing (1+ (/ extra-spacing default-width))
		    extra-spacing (- extra-spacing
				     (* fix-spacing default-width))))
	     ((> (* -2 extra-spacing) default-width)
	      (setq fix-spacing (1- (/ extra-spacing default-width))
		    extra-spacing (- extra-spacing
				     (* fix-spacing default-width))))
	     (t
	      (setq fix-spacing 0)))
	    (if key-list 
		(princ (make-string (max
				     0
				     (- trunc-length
				       (+ 2 (/ 2 trunc-length))
				       fix-spacing)) ?\ ))))
	(princ (substring (concat (if (stringp key-def) key-def "")
				  "         ")
			  0 trunc-length))
	(if key-list (princ " "))))
    (terpri)
    (princ (car row))
    (princ (mapconcat
	    (function (lambda (x)
			(concat key-space-before
				(char-to-string
				 (if (memq arg '(plain
						 meta
						 control
						 meta-control))
				     (car x)
				   (car (cdr x))))
				key-space-after)))
	    (cdr row) " "))
    (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dollar sign.
;;

(defvar ultex-dollar-last -1
  "Index of element of ultex-dollar-array most recently inserted.")

(defvar ultex-current-label ""
  "Last label used in ultex-smart-dollars and ultex-change-dollars")

(defun ultex-dollar-increment (i)
  (setq i (1+ i))
  (if (>= i (ultex-dollar-max))
      ultex-dollar-back
    i))

(defun ultex-insert-dollars ()
  "Make a $ pair."
  (interactive)
  (let* ((next (ultex-dollar-increment ultex-dollar-last))
	 p str)
    (if (< ultex-dollar-last 0) nil
      (setq p (aref ultex-dollar-array ultex-dollar-last)
	    str (car p))
      (save-excursion
	(search-backward (ultex-dollar-entry-start p) nil t)
	(if (looking-at (regexp-quote str))
	    (delete-char (length str))
	  (setq next 0))))
    (setq p (aref ultex-dollar-array next)
	  str (car p))
    (or (= next 0) (zerop (current-column))
	  (newline))
    (insert (car p))
    (ultex-forward-char (cdr p))
    (setq ultex-current-label ""
	  ultex-dollar-last next)))

(defun ultex-change-dollars ()
  "Change the math environment starting and ending string, according
to the entries of ultex-dollar-array."
  (interactive)
  (let* ((next (ultex-dollar-increment ultex-dollar-last))
	 old old-start old-end old-label
	 new new-start new-end new-label
	 offset beg end beg-newline end-newline)
    (setq old (car (aref ultex-dollar-array ultex-dollar-last))
	  old-start (ultex-dollar-entry-start (cons old t))
	  old-end (ultex-dollar-entry-start (cons old t) 'end)
	  old-label (string-match "\\\\label{}" old)
	  offset (point)
	  new (car (aref ultex-dollar-array next))
	  new-start (ultex-dollar-entry-start (cons new t))
	  new-end (ultex-dollar-entry-start (cons new t) 'end)
	  new-label (string-match "\\\\label{}" new))
    (save-excursion (search-backward old-start nil t)
		    (setq beg (point)
			  beg-newline (zerop (current-column)))
		    (goto-char (match-end 0))
		    (search-forward old-end nil t)
		    (setq end (point))
		    (while (looking-at "[ \t]") (delete-char 1))
		    (ultex-forward-char 1)
		    (setq end-newline (zerop (current-column))))
    (unwind-protect
	(progn
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (if (and old-label
		   (save-excursion
		     (search-forward "\\label{" nil t)))
	      (setq old-start
		    (concat old-start "\\label{" ultex-current-label "}")))
	  (delete-char (length old-start))
	  (if (= next 0)
	      (while (looking-at "\n") (delete-char 1))
	    (or beg-newline
		(progn 
		  (setq offset (1+ offset))
		  (newline))))
	  (if new-label
	      (setq new-start
		    (concat new-start "\\label{" ultex-current-label "}")))
	  (setq offset (+ (max (- offset (length old-start))
			       (1+ beg))
			  (length new-start)))
	  (if (and (null (string-match (regexp-quote "\n") old))
		   (string-match (regexp-quote "\n") new))
	      (setq new-start (concat new-start "\n")
		    new-end (concat "\n" new-end)
		    offset (1+ offset)))
	  (insert new-start)
	  (search-forward old-end nil t)
	  (goto-char (match-beginning 0))
	  (delete-char (length old-end))
	  (save-excursion
	    (if (and (zerop next)
		     (progn
		       (backward-char 1)
		       (looking-at "\n")))
		(delete-char 1)))
	  (insert new-end)
	  (or (= next 0)
	      end-newline
	      (newline))
	  (search-backward new-start nil t)
	  ;;(if (and new-label
	  ;;       (string= ultex-current-label ""))
	  ;;    (progn
	  ;;	(goto-char (point-min))
	  ;;	(search-forward "\\label{" nil t))
	  ;;(goto-char offset))
	  (goto-char offset)
	  (setq ultex-dollar-last next))
      (widen))))

(defun ultex-dollar-entry-start (entry &optional end)
  "Given element ENTRY of ultex-dollar-array, return the part of it
that starts the math environment (not including the label).  If
optional arg END is non-nil, return the end of it, rather than the
beginning."
  (if (string= "$$" (car entry)) "$"
    (let ((temp (substring (car entry) 0
			   (string-match "\n\\|\\\\label{}\\|\\\\end"
					 (car entry)))))
      (if end
	  (substring (car entry)
		     (progn
		       (string-match
			(concat (regexp-quote temp)
				"\\(\\\\label{}\\)?[ \n]*")
			(car entry))
		     (match-end 0)))
	temp))))

(defun ultex-smart-dollars nil
  "Call either ultex-change-dollars or ultex-insert-dollars, depending
on whether the point is in math mode due to an element of
ultex-dollar-array."
  (interactive)
  (let ((ultex-starting-array (mapcar 'ultex-dollar-entry-start
				      ultex-dollar-array))
	(math-mode (save-excursion (skip-chars-backward "\n\t ")
				   (ultex-math-mode t)))
	start-regexp temp dummy pt)
    (setq start-regexp
	  (mapconcat 'regexp-quote ultex-starting-array "\\|")
	  temp -1)
    (if (eq math-mode 'def)
	(unwind-protect
	    (let (beg end)
	      (save-excursion
		(re-search-backward ultex-def-regexp nil t)
		(goto-char (match-end 0))
		(setq beg (point))
		(forward-sexp 1)
		(setq end (point)))
	      (narrow-to-region beg end)
	      (ultex-smart-dollars))
	  (widen))
      (if (and math-mode
	       (save-excursion
		 (re-search-backward start-regexp nil t))
	       (save-excursion
		 (re-search-backward
		  start-regexp (1- (match-beginning 0)) t))
	       (setq dummy (- (ultex-dollar-max)
			      (length (member
				       (buffer-substring-no-properties
					(match-beginning 0)
					(match-end 0))
				       ultex-starting-array))))
	       (if (string-match
		    "\\\\label"
		    (car (aref ultex-dollar-array dummy)))
		   (setq ultex-current-label
			 (save-excursion
			   (search-forward
			    (ultex-dollar-entry-start
			     (aref ultex-dollar-array dummy)
			     'end) nil t)
			   (setq pt (point))
			   (re-search-backward start-regexp nil t)
			   (if (re-search-forward
				"\\\\label{\\([^}]*\\)}" pt t)
			       (buffer-substring-no-properties
				(match-beginning 1)
				(match-end 1))
			     "")))
		 t)
	       (if (and (string= "" ultex-current-label)
			(save-excursion
			  (re-search-backward start-regexp nil t)
			  (looking-at (regexp-quote
				       (car (aref ultex-dollar-array
						  dummy))))))
		   (setq temp dummy
			 dummy nil)
		 t))
	  (if (save-excursion
		(let (end)
		  (if (string= "$" (ultex-dollar-entry-start
				    (aref ultex-dollar-array dummy)))
		      (save-excursion
			(forward-paragraph)
			(setq end (point)))
		    (setq end (point-max)))
		  (setq temp dummy
			dummy (ultex-dollar-entry-start
			       (aref ultex-dollar-array temp)
			       'end))
		  (if (re-search-forward (concat (regexp-quote "$") "\\|"
						 ultex-math-end)
					 end t)
		      (progn (backward-char (length dummy))
			     (looking-at (regexp-quote dummy))))))
	      (if (and (string= "$" dummy)
		       (not (ultex-math-mode t)))
		  (progn
		    (message "There seems to be an unmatched $ in the previous paragraph.")
		    (ding)
		    (setq ultex-dollar-last -1)
		    (ultex-insert-dollars))
		(setq ultex-dollar-last temp)
		(ultex-change-dollars))
;	    (save-excursion (insert dummy)))
	    (insert dummy))
	(setq ultex-dollar-last temp)
	(if (save-excursion (and ultex-slash-dollar-is-not-math
				 (ultex-forward-char -1)
				 (looking-at "\\\\")
				 (ultex-forward-char -1)
				 (looking-at "[^\\]")))
	    (insert "$")
	  (ultex-insert-dollars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous utilities.
;;

(defun ultex-commented-outp nil
  "Return t is comment character is found between bol and point.
I stole this from font-latex.el."
  (save-excursion
    (let ((limit (point)))
      (save-match-data
        (re-search-backward "^\\|\C-m" (point-min) t)
	(if (re-search-forward "^%\\|[^\\]%" limit t)
	    t
          nil)))))

(defun ultex-re-search-forward (regexp &optional bound noerror count)
  "Like re-search-forward, but ignore lines that are commented out.
If successful, return buffer position (cf. re-search-forward in Emacs 18)."
  (let ((pt (re-search-forward regexp bound noerror count)))
    (while (and pt
		(ultex-commented-outp))
      (end-of-line nil)
      (setq pt (re-search-forward regexp bound noerror count)))
    (and pt (point))))

(defun get-latex-skel nil
  "Read in a LaTeX outline from ultex-latex-skel-file."
  (interactive)
  (insert-file ultex-latex-skel-file)
  (run-hooks 'ultex-skel-hook))

(defun ultex-font (font)
  "Make a brace pair with font."
  (cond ((equal font "cal")
	 (insert (concat "\\mathcal{"
			 (char-to-string
			  (upcase (progn (message "{\\cal ?}:")
					 (read-char))))
			 "}")))
	((equal font "up")
	 (progn 
	   (insert "\\textup{")
	   (save-excursion (insert "}"))))
	((equal font "text")
	 (progn
	   (ultex-math-mode)
	   (insert "\\text{")
	   (save-excursion (insert "}"))))
	((or (equal font "rm") (equal font "bf") (equal font "sc")
	     (equal font "sf") (equal font "it") (equal font "sl")
	     (equal font "tt"))
	 (progn
	   (insert (concat (if (ultex-math-mode t) "\\math" "\\text")
			   font "{"))
	   (save-excursion (insert "}"))))
	((equal font "em")
	 (progn
	   (insert (concat "\\emph{"))
	   (save-excursion (insert "}"))))
	(t
	 (insert (concat "\\" font "{"))
	 (save-excursion (insert "}"))))
  (if (equal font "cal") (ultex-math-mode)))

;; Notes: amsmath provides several dots commands: \dotsc for use with
;; commas (as in "$x_1, x_2, \dotsc$"), \dotsb for use with binary
;; operators like + (as in "$x_1 + x_2 + \dotsb$"), and \dotsm for use
;; with multiplication (as in "$x_1 x_2 \dotsm$").  \dotsm is a
;; synonym for \dotsb, and I don't know how to program ultra-tex to
;; recognize when to use one over the other, so I only use \dotsb.  
;;   \dots is a TeX command for use in text mode: it inserts an
;; ellipsis.  amsmath extends this command to math mode, in which
;; case it looks at the following character to try to determine
;; whether it should insert \dotsb or \dotsc.  (So $x + \dotsb + y$
;; and $x + \dots + y$ should yield the same result.)  When the dots
;; are at the end of the expression (see my examples in the previous
;; paragraph), \dots inserts \dotsc.  (Hence $x_1 + \dots$ and
;; $x_1 + \dotsb$ do different things.)  So it might be best to avoid
;; using \dots in math mode, and to use \dotsb and \dotsc instead.
;;   amsmath also provides the command \dotsi for use after integrals,
;; as in "$\int_A \int_B \dotsi$".  I might implement this eventually.
(defun ultex-maybe-dots nil
  "`.' unless it follows `..', then appropriate \\dots command.
The effect is controlled by the customizable variable ultex-dots-behavior."
  (interactive)
  (if (or (< (current-column) 2)
	  (not ultex-dots-behavior)
	  (not (save-excursion (forward-char -2) (looking-at "\\.\\."))))
      (insert ".")
    (delete-char -2)
    (if (not (ultex-math-mode t))
	(insert "\\dots ")
      (if (and (save-excursion
		 (re-search-backward
		  ",\\s-*\\|\\W\\s-+\\|[^\\]\\b\\w+\\s-+"
		  (- (point) 30) t))
	       (= (match-end 0) (point)))
	  (if (and (save-excursion (re-search-backward ",\\s-*" nil t))
		   (= (match-end 0) (point)))
	      (cond
	       ((eq ultex-dots-behavior 'latex)
		(insert "\\ldots "))
	       ((eq ultex-dots-behavior 'ams)
		(insert "\\dotsc "))
	       ((eq ultex-dots-behavior 'dots)
		(insert "\\dots ")))
	    (cond 
	     ((eq ultex-dots-behavior 'latex)
	      (insert "\\cdots "))
	     ((eq ultex-dots-behavior 'ams)
	      (insert "\\dotsb "))
	     ((eq ultex-dots-behavior 'dots)
	      (insert "\\dots "))))
	(cond 
	 ((eq ultex-dots-behavior 'latex)
	  (insert "\\cdots "))
	 ((eq ultex-dots-behavior 'ams)
	  (insert "\\dotsb "))
	 ((eq ultex-dots-behavior 'dots)
	  (insert "\\dots ")))))
    (if (> (current-column) fill-column) (run-hooks 'auto-fill-hook))))

(defun ultex-superscript nil
  "Set up for superscript."
  (interactive)
  (ultex-math-mode)
  (if ultex-subscript-delete-trailing-space
      (save-excursion
	(ultex-forward-char -1)
	(if (looking-at " ")
	    (delete-char 1))))
  (insert "^{")
  (save-excursion (insert "}")))

(defun ultex-subscript nil
  "Set up for subscript."
  (interactive)
  (ultex-math-mode)
  (if ultex-subscript-delete-trailing-space
      (save-excursion
	(ultex-forward-char -1)
	(if (looking-at " ")
	    (delete-char 1))))
  (insert "_{")
  (save-excursion (insert "}")))

(defun ultex-par nil
  "Two newlines and validate if this looks like the end of paragraph,
otherwise plain newline."
  (interactive)
  (if (and (eolp) (not (bolp)) (not (ultex-math-mode t))
	   (not (ultex-macro-line-p)))
      (ultex-terminate-paragraph nil)
    (newline)))

;; From Ed Reingold, and no, we're not related.
(defun tex-last-unended-begin ()
  "Leave point at the beginning of the last \\begin{...} that is unended."
  (while (and (re-search-backward "\\(\\\\begin{\\)\\|\\(\\\\end{\\)")
	      (or (looking-at "\\\\end{")
		  (ultex-commented-outp)))
    (tex-last-unended-begin)))

(defun latex-environment ()
  (let (not-found)
    (save-excursion
      (condition-case nil
	  (tex-last-unended-begin)
	(error (setq not-found t)))
      (if (or not-found
	      (not (re-search-forward
		    "\\\\begin{\\([^}\n]*\\)}"
		    nil
		    t)))
	  nil
	(intern (buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))

(defun ultex-next-item ()
  "After end of an item in an itemize-type environment, drop any \\\\
on the previous line, and start this line with `\\item []' or `\\item ',
depending on the value of the ultex-item-insert-brackets."
  (and (memq (latex-environment) ultex-itemizing-environments)
       (progn (and (save-excursion 
		     (ultex-forward-char -3)
		     (looking-at "\\\\\\\\\n"))
		   (progn (delete-backward-char 3) 
			  (insert "\n")))
	      (if ultex-item-insert-brackets
		  (progn 
		    (insert "\\item []")
		    (ultex-forward-char -1))
		(insert "\\item ")))))

(defun ultex-insert-quote (count)
  "Insert ``, '' or \" according to preceding character.
With numeric arg N, always insert N \" characters."
  (interactive "P")
  (if count
      (if (numberp count)
	  (self-insert-command count)
	(self-insert-command 1))
    (save-excursion
      (if (ultex-forward-char -1)
	  (if (looking-at "\"")
	      (delete-char 1))))
    (cond
     ((or (bobp)
	  (save-excursion
	    (ultex-forward-char -1)
	    (looking-at "[ \t\n]\\|\\s(")))
      (insert "``"))
     ((save-excursion
	(ultex-forward-char -2)
	(looking-at "''\\|``"))
      (ultex-forward-char -2)
      (delete-char 2)
      (insert "\""))
     ((= (preceding-char) ?\\)
      (insert ?\"))
     (t (insert "''")))))

(defun ultex-comment-indent nil
  (if (looking-at "\\s<\\s<")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column))) comment-column)))

(defun ultex-insert-braces nil
  "Make a pair of braces and be poised to type inside of them."
  (interactive)
  (insert ?\{)
  (save-excursion
    (if (and ultex-dont-use-completion
	     (save-excursion
	       (ultex-forward-char -3)
	       (looking-at "[^\\]\\\\{")))
	(insert "\\}")
      (insert ?}))))

(defun ultex-insert-brackets nil
  "Make a pair of brackets and be poised to type inside of them."
  (interactive)
  (insert ?[)
  (save-excursion
    (insert ?])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions stolen (and corrected) from TeX mode.
;;

;; The following fixes a bug that appears to be a problem when
;; 'forward-sexp has an argument > 1
(defun ultex-validate-paragraph (start end)
  (condition-case nil
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
;	  (forward-sexp (- end start))	; original version--it fails
	  (while (not (eobp)) (forward-sexp)) ; works, more efficiently too
	  t))
    (error nil)))

;; This fixes the bug on newline in auto-fill-mode
(defun ultex-terminate-paragraph (inhibit-validation)
  "Insert two newlines, breaking a paragraph for TeX.
Check for mismatched braces/$'s in paragraph being terminated.
A prefix arg inhibits the checking."
  (interactive "P")
  (or inhibit-validation
      (ultex-validate-paragraph
       (save-excursion
	 (search-backward "\n\n" nil 'move)
	 (point))
       (point))
      (progn 
	(message "Paragraph being closed appears to contain a mismatch")
	(ding)))
  (newline) (newline))			; (insert "\n\n") doesn't auto-fill

;; imenu support, stolen from tex-mode.el
(defun ultex-imenu-create-index ()
  "Generates an alist for imenu from an ultra-tex buffer."
  (let (result temp)
    (goto-char (point-max))
    (while (re-search-backward "\\\\\\(part\\|chapter\\|\
\\(sub\\)?\\(\\(sub\\)?section\\|paragraph\\)\\)\\*?[ \t\n]*{\\([^}]*\\)}" nil t)
      (setq temp
	    (assoc (buffer-substring-no-properties (match-beginning 1)
						   (match-end 1)) 
		   '(("part" . "") ("chapter" . " ")
		     ("section" . "  ") ("subsection" . "   ")
		     ("subsubsection" . "    ")
		     ("paragraph" . "     ") ("subparagraph" . "      "))))
      (setq result (cons (cons (concat (cdr temp) (match-string 5)) 
			       (match-beginning 0))
			 result)))
    result))

(defvar ultex-path-separator ":"
  "Character used to separate concatenated paths.")

(defun ultex-parse-colon-path (cd-path)
  "Explode a colon-separated list of paths into a string list.
This differs from parse-colon-path in that it treats a trailing //
appropriately--if a path name ends in //, then that directory and all
of its subdirectories, recursively, are added to the list.  All
directory names in the list will end with a slash."
  (and cd-path
       (let (cd-list
	     (cd-start 0)
	     cd-colon
	     path
	     path-list)
	 (setq cd-path (concat cd-path ultex-path-separator))
	 (while (setq cd-colon
		      (string-match ultex-path-separator
				    cd-path
				    cd-start))
	   (setq path (substring cd-path cd-start cd-colon))
	   (if (eq (string-match "//" path)
		   (- (length path) 2))
	       (setq path-list (cons (substitute-in-file-name
				 (file-name-as-directory
				  (substring path 0 -1)))
				     (ultex-all-subdirs
				      (substitute-in-file-name
				       (file-name-as-directory
					(substring path 0 -1))))))
	     (if (= cd-start cd-colon)
		 (setq path-list nil)
	       (setq path-list (list (substitute-in-file-name
				      (file-name-as-directory
				       (substring cd-path cd-start
						  cd-colon)))))))
	   (setq cd-list (nconc cd-list path-list)
		 cd-start (+ cd-colon 1)))
	 cd-list)))

(defun ultex-all-subdirs (path)
  "Return list of all subdirectories of PATH, recursively, omitting
'.' and '..'."
  (if (file-directory-p path)
      (let ((file-list (directory-files path nil))
	    file
	    answer)
	(while file-list
	  (setq file (car file-list)
		file-list (cdr file-list))
	  (or (string= file ".")
	      (string= file "..")
	      (and (file-directory-p (concat path file))
		   (setq answer (cons (concat path (file-name-as-directory file)) answer)
			 file (ultex-all-subdirs (file-name-as-directory
						  (concat path file))))
		   (setq answer (append file answer)))))
	answer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff dependent on the version of Emacs.
;;

(defmacro ultex-menu-add (&rest menus)
  `(ultex-menu-add-1 ',menus))
(put 'ultex-menu-add 'lisp-indent-function 1)

(defun ultex-menu-add-1 (menus)
  (when (featurep 'menubar)
    (while menus
      (easy-menu-add (symbol-value (pop menus))))))

(defun ultex-menu-define-xemacs nil
  "Set up Ultra-TeX menus in XEmacs."
  (ultex-menu-add ultra-tex-menu))

;; caddr and cadddr aren't defined in GNU Emacs, at least in version 20.3.
(if (not (and (fboundp 'caddr) (fboundp 'cadddr) (fboundp 'cddddr)))
    (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ultex)

;;; ultex.el ends here
