;;; ultex-cus.el --- customization things in Ultra-TeX mode
;; Copyright (c) 1998, 1999, 2000, 2001, 2002
;; Mark Haiman, Nick Reingold, John Palmieri

;; Authors:   Mark Haiman <mhaiman@macaulay.ucsd.edu>, 
;;            Nick Reingold, 
;;            John Palmieri <palmieri@math.washington.edu>
;; Maintainer: John Palmieri <palmieri@math.washington.edu>
;;             URL: http://www.math.washington.edu/~palmieri/Emacs/ultratex.html
;; Keywords: TeX-mode, completion
;; Version:  0.71 of Wed Jun  5 22:21:34 PDT 2002

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
;; This file contains all of the customizable things for Ultra-TeX
;; mode.  If you are using Emacs 20, or an earlier version of Emacs
;; which has the customization package installed, you can change all
;; of the relevant variables here via customization.  This is
;; preferable to doing it "by hand" in your .emacs file.

(require 'light)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization groups
;;

(defgroup ultra-tex nil
  "Ultra-TeX mode: major mode for editing LaTeX documents."
  :tag "Ultra-TeX"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'tex)

(defgroup ultra-tex-init nil
  "Initialization options for Ultra-TeX mode."
  :tag "Ultra-TeX initialization"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

(defgroup ultra-tex-files nil
  "Files used by Ultra-TeX mode."
  :tag "Ultra-TeX files"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

(defgroup ultra-tex-commands nil
  "TeX/LaTeX commands in Ultra-TeX mode."
  :tag "Ultra-TeX commands"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

(defgroup ultra-tex-parens nil
  "Parentheses, braces, brackets in Ultra-TeX mode."
  :tag "Parentheses in Ultra-TeX mode"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

(defgroup ultra-tex-environments nil
  "Regular expressions for environments and math mode in Ultra-TeX mode. "
  :tag "Ultra-TeX environments"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

(defgroup ultra-tex-misc nil
  "Ultra-TeX mode miscellany."
  :tag "Ultra-TeX miscellany"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load init file first.

(defcustom ultex-init-file "~/.ultex"
  "Your Ultra-TeX Emacs lisp startup file.
If you are setting Ultra-TeX options with customize, this file may be
superfluous.
If a file with the .el or .elc suffix exists, it will be read
instead."
  :type '(file)
  :group 'ultra-tex-init)

(load ultex-init-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex
;;

(defcustom ultex-complete-idle-time lc-complete-idle-time-default
  "*After this much idle time has elapsed, try to complete.  Measured
in seconds."
  :type '(number)
  :group 'ultra-tex)

(make-variable-buffer-local 'ultex-complete-idle-time)

(defcustom ultra-tex-mode-hook nil
  "Hook for Ultra TeX mode."
  :type '(hook)
  :options '(turn-on-auto-fill)
  :group 'ultra-tex)

(defcustom ultex-run-text-mode-hook nil
  "Toggle whether to run text-mode-hook upon entering Ultra TeX mode.
If non-nil, the contents of text-mode-hook get executed before running
tex-mode-hook and ultra-tex-mode-hook."
  :type '(boolean)
  :group 'ultra-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex-files
;;

(defvar ultex-guess-directory
  (if (locate-library "ultex-cus")
      (concat
       (substring (locate-library "ultex-cus")
		  0
		  (- (string-match "ultex-cus"
				   (locate-library "ultex-cus"))
		     5))
       "user/")
    ""))

(defcustom ultex-tree-file
  (concat ultex-guess-directory "textree.el")
  "Name of file containing the list of TeX commands."
  :type '(file :must-match t)
  :group 'ultra-tex-files)

(defcustom ultex-latex-skel-file
  (concat ultex-guess-directory "skeleton.tex")
  "File containing template for latex document, inserted by the
function `get-latex-skel'."
  :type '(file :must-match t)
  :group 'ultra-tex-files)

(defcustom ultex-greek-keys-file
  (concat ultex-guess-directory "greek.el")
  "File defining the greek keymap."
  :type '(file :must-match t)
  :group 'ultra-tex-files)

(defcustom ultex-skel-hook nil
  "This hook is called when ultex-latex-skel-file is inserted by
the function `get-latex-skel'."
  :type '(hook)
  :group 'ultra-tex-files)

(defcustom ultex-extra-tree-files nil
  "List of triples--the first element is a class, style, or package,
the second is an associated file of TeX commands, the third is a flag.
When that class, style, or package is used in any given document, the
associated file of TeX commands is loaded.  If the flags are nil for
every class, style, and package used in the document, then the main
TeX command file (`ultex-tree-file') is not loaded."
  :type '(repeat
	  (list (string :tag "Class/style/package")
		(file :must-match t :tag "Command file")
		(boolean :tag "Also load main command file?")))
  :group 'ultra-tex-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex-init
;;

(defvar ultra-tex-mode-internal-hook nil
  "A hook for Ultra TeX mode.  This one is set according to the values
of various customizable variables.  It should not be set by
hand--users should set ultra-tex-mode-hook instead.")

(defconst lc-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version)
  "Non-nil if using XEmacs")

(defcustom ultex-use-color nil
  "*Toggle use of color in Ultra-TeX mode.
This is pretty, but it can add a noticeable delay when starting
ultra-tex-mode."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if (and window-system
		  (x-display-color-p)
		  value)
	     (add-hook 'ultra-tex-mode-internal-hook
		       'turn-on-font-lock)
	   (remove-hook 'ultra-tex-mode-internal-hook
			'turn-on-font-lock))
	 (set symbol value))
  :group 'ultra-tex-init)

(defcustom ultex-use-font-latex nil
  "*Toggle use of the font-latex package for colorization.
This variable is only relevant if ultex-use-color is `on'.
One version of the file font-latex.el is distributed with Ultra-TeX.
The latest recent version is available from  
  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/font-latex.el"
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (progn
	       (require 'font-lock)
	       (require 'font-latex)
	       (setq font-latex-keywords font-latex-keywords-2)
	       (if lc-xemacs-p
		   (put 'ultra-tex-mode 'font-lock-keywords
			'font-latex-keywords)
		 (setq font-lock-defaults-alist
		       (cons (cons 'ultra-tex-mode
				   '(font-latex-keywords
				     nil nil ((?$ . "\""))))
			     font-lock-defaults-alist))))
	   (or lc-xemacs-p
	       (require 'font-lock)
	       (setq font-lock-defaults-alist
		     (cons (cons
			    'ultra-tex-mode
			    '((tex-font-lock-keywords
			       tex-font-lock-keywords-1
			       tex-font-lock-keywords-2)
			      nil nil ((?$ . "\"")) nil
			      (font-lock-mark-block-function
			       . mark-paragraph)))
			   font-lock-defaults-alist))))
	 (set symbol value))
  :link '(url-link
	  :tag
	  "FTP site"
	  "ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/font-latex.el")
  :group 'ultra-tex-init)

(defcustom ultex-use-auctex nil
  "*Toggle use of files from AUC-TeX distribution.
This has one main effect: several commands are added to Ultra-TeX to
allow one to typeset the current TeX document, spawn an external
viewer, jump to the next error, etc.

This option requires the use of the file tex-buf.el from the AUC-TeX
distribution.  This file is included with Ultra-TeX, but if you want
the most up-to-date version, the AUC-TeX package is available from
  http://sunsite.auc.dk/auctex/"
  :type '(boolean)
  :set (lambda (symbol value)
	 (let ((file (locate-library "tex-buf")))
	   (if (and value (stringp file))
	       (progn
		 (defvar TeX-lisp-directory
		   (file-name-directory file)
		   "*The directory where the AUC TeX lisp files are located.")
		 (add-hook 'ultra-tex-mode-internal-hook
			   'ultra-tex-require-aucify))
	     (remove-hook 'ultra-tex-mode-internal-hook
			  'ultra-tex-require-aucify)))
	 (set symbol value))
  :link '(url-link :tag "Home Page" "http://sunsite.auc.dk/auctex/")
  :group 'ultra-tex-init)

(defun ultra-tex-require-aucify ()
  "Require aucify.  For use in ultra-tex-mode-internal-hook."
  (require 'aucify))

(defcustom ultex-use-imenu nil
  "Toggle use of imenu package in Ultra-TeX mode.
The imenu package lets you pop up a menu giving an outline of
the buffer, so that you can move around to various section headings
and theorem statements and so forth.  If this option is on, then
S-mouse-3 (hold down the shift key, and hit (and release) the right
mouse button) runs the command imenu, which pops up the aforementioned
menu."
  :type '(boolean)
  :set (lambda (symbol value)
	 (if (and window-system value)
	     (progn
	       (require 'imenu)
	       (if lc-xemacs-p
		   (global-set-key [(shift button3)] 'imenu)
		 (global-set-key [S-mouse-3] 'imenu))
	       (add-hook 'ultra-tex-mode-internal-hook
			 'ultra-tex-add-imenu))
	   (remove-hook 'ultra-tex-mode-internal-hook
			'ultra-tex-add-imenu))
	 (set symbol value))
  :group 'ultra-tex-init)

(defun ultra-tex-add-imenu ()
  "imenu stuff.  For use in ultra-tex-mode-internal-hook."
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'ultex-imenu-create-index))

(defcustom ultex-use-bib-cite nil
  "Toggle use of bib-cite package in Ultra-TeX mode.
The bib-cite package does stuff with your labels and
citations--highlights them, lets you jump to them, etc.  This option
is not well-tested, and may yield unpredictable results.  The latest
version of bib-cite is available from
  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/bib-cite.el"
  :type '(boolean)
  :set (lambda (symbol value)
	 (if value
	     (progn 
	       (require 'bib-cite)
	       (remove-hook 'ultra-tex-mode-internal-hook
			    'ultra-tex-add-imenu)
	       (add-hook 'ultra-tex-mode-internal-hook
			 'ultra-tex-add-bib-cite)
	       (if (and window-system ultex-use-imenu)
		   (add-hook 'ultra-tex-mode-internal-hook
			     'ultra-tex-add-bib-cite-imenu)))
	   (remove-hook 'ultra-tex-mode-internal-hook
			'ultra-tex-add-bib-cite)
	   (remove-hook 'ultra-tex-mode-internal-hook
			'ultra-tex-add-bib-cite-imenu))
	 (set symbol value))
  :link '(url-link
	  :tag
	  "FTP site"
	  "ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/bib-cite.el")
  :group 'ultra-tex-init)

(defun ultra-tex-add-bib-cite-imenu ()
  "bib-cite, imenu stuff.  For use in ultra-tex-mode-internal-hook."
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu--create-LaTeX-index))

(defun ultra-tex-add-bib-cite ()
  "bib-cite stuff.  For use in ultra-tex-mode-internal-hook."
  (define-key ultra-tex-mode-map "\C-cb" bib-cite-map)
  (bib-cite-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex-commands
;;

(defcustom ultex-alist-local-flag t
  "*If non-nil, each Ultra-TeX buffer maintains its own list of TeX
commands, labels, and bibliographic information."
  :type '(boolean)
  :group 'ultra-tex-commands)

(defcustom ultex-no-grab-if-space nil
  "If non-nil, ignore all \\newcommands (etc.) that do not start at
the beginning of the line."
  :type '(boolean)
  :group 'ultra-tex-commands)

(defcustom ultex-rescan-after-new-label nil
  "If non-nil, rescan buffer for TeX commands after entering new \\label.
You might want to do this if you change labels a lot, and you don't
want to complete on the old labels.  This variable has no effect if
ultex-alist-local-flag is nil (you don't want to reset the labels
because there may be some valid ones in another buffer)."
  :type '(boolean)
  :group 'ultra-tex-commands)

(make-variable-buffer-local 'ultex-rescan-after-new-label)

(defcustom ultex-return-checks-for-new-commands t
  "If non-nil, scan current line after hitting RET for a newcommand
or similar thing (renewcommand, DeclareMathOperator, newenvironment,
etc.).  This should be quick, so the default value it t."
  :type '(boolean)
  :group 'ultra-tex-commands)

(defcustom ultex-append-space-to-defs t
  "If non-nil, add a space to TeX control sequences."
  :type '(boolean)
  :group 'ultra-tex-commands)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex-parens
;;

(defcustom ultex-match-braces t
  "If non-nil, hitting `{' inserts `{}'"
  :type '(boolean)
  :group 'ultra-tex-parens)

(defcustom ultex-match-brackets nil
  "If non-nil, hitting `[' inserts `[]'"
  :type '(boolean)
  :group 'ultra-tex-parens)

(defcustom ultex-match-parens nil
  "If non-nil, hitting `(' inserts `()'"
  :type '(boolean)
  :group 'ultra-tex-parens)

(defcustom ultex-blink-matching-paren t
  "If non-nil, hitting ')' will briefly highlight the matching '('.
More to the point, if this is nil, then the syntax type of parentheses
is changed, in which case Ultra-TeX might do a better job of
determining when the point is in math mode.  If you don't want to mess
with the syntax table, but you do want to control the blinking of
matching parentheses, look at the variable `blink-matching-paren'
instead."
  :type '(boolean)
  :group 'ultra-tex-parens)

(defcustom ultex-blink-matching-bracket t
  "If non-nil, hitting ']' will briefly highlight the matching '['.  
More to the point, if this is nil, then the syntax type of brackets is
changed, in which case Ultra-TeX might do a better job of determining
when the point is in math mode.  If you don't want to mess with the
syntax table, but you do want to control the blinking of matching
parentheses, look at the variable `blink-matching-paren' instead."
  :type '(boolean)
  :group 'ultra-tex-parens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex-environments
;;

(defcustom ultex-array-environs
  '(array tabular eqnarray eqnarray* align gather alignat aligned gathered
	      alignedat split CD matrix smallmatrix pmatrix bmatrix vmatrix
	      Vmatrix xalignat xxalignat cases)
  "List of symbols of array environments"
  :type '(repeat symbol)
  :group 'ultra-tex-environments)

(defcustom ultex-math-start
  "\\\\\\[\\|\\\\(\\|\\\\begin{\\(eq[a-z*]*\\|x*align\\(at\\)?\\*?\\|gather\\*?\\|multline\\*?\\|[a-z]*math\\)}\\(\\\\label{[^}]}\\)?"
  "Regular expression to match start of math mode"
  :type '(regexp)
  :group 'ultra-tex-environments)

(defcustom ultex-math-end
  "\\\\\\]\\|\\\\)\\|\\\\end{\\(eq[a-z*]*\\|x*align\\(at\\)?\\*?\\|gather\\*?\\|multline\\*?\\|[a-z]*math\\)}"
  "Regular expression to match end of math mode"
  :type '(regexp)
  :group 'ultra-tex-environments)

(defcustom ultex-itemizing-environments
  '(list trivlist itemize description enumerate)
  "List of symbols of LaTeX environments which use \\item."
  :type '(repeat symbol)
  :group 'ultra-tex-environments)

(defcustom ultex-item-insert-brackets t
  "Toggle whether to insert `\\item []' or `\\item ' in itemizing environments."
  :type '(boolean)
  :group 'ultra-tex-environments)

(defcustom ultex-math-cs-regexp nil
  "Regexp to match TeX commands whose arguments are set in math mode."
  :type '(regexp)
  :group 'ultra-tex-environments)

(defcustom ultex-non-math-cs-regexp "\\\\[a-z]*box{\\|\\\\text[a-z]*{"
  "Regexp to match TeX commands whose arguments are NOT set in math mode."
  :type '(regexp)
  :group 'ultra-tex-environments)

(defcustom ultex-def-regexp
  "\\\\\\(\\(re\\)?new\\|provide\\)command\\s-*{\\(\\\\[a-zA-Z]+\\|\\\\.\\)\\s-*}\\(\\[[1-9]\\]\\)?"
  "Regexp to match TeX commands whose arguments may or may not be in math mode."
  :type '(regexp)
  :group 'ultra-tex-environments)

(defcustom ultex-dollar-array-cursor "|"
  "String to use to mark the cursor position in ultex-dollar-array."
  :type '(string)
  :group 'ultra-tex-environments)

(defun ultex-convert-dollar-array-external (list)
  "Convert LIST (this argument should be ultex-dollar-array-external)
to vector of (string . integer) pairs."
  (let ((temp list)
	(answer (make-vector (length list) nil))
	(index -1)
	string
	cursor-pos)
    (while temp
      (setq string (ultex-dollar-guess-cursor (car temp))
	    temp (cdr temp)
	    index (1+ index)
	    cursor-pos (string-match
			(regexp-quote ultex-dollar-array-cursor)
			string))
      (setq string
	    (concat (substring string 0 cursor-pos)
		    (if (< cursor-pos (length string))
			    (substring string (1+ cursor-pos)))))
      (aset answer index (cons string (- cursor-pos
					 (length string)))))
    answer))

(defun ultex-dollar-guess-cursor (str)
  "Given string STR, return string with ultex-dollar-array-cursor
inserted in appropriate place: if already present, return STR
unchanged.  If STR has substring \label{}, put cursor inside braces.
Else if STR has substring \begin{blah}, put cursor after \begin{blah}.
Else put cursor at end of string."
  (let ((num (string-match (regexp-quote ultex-dollar-array-cursor)
			    str)))
    (if num str
      (setq num (string-match "\\\\label{}" str))
      (if num (setq num (+ num 7))
	(setq num (string-match
		   "\\\\begin{[^}]*}\n" str))
	(if num (setq num (match-end 0))
	  (setq num (length str))))
      (concat (substring str 0 num)
	      ultex-dollar-array-cursor
	      (substring str num)))))

(defcustom ultex-dollar-array-external
  '("$|$"
    "\\[\n|\n\\]"
    "\\begin{equation}\\label{|}\n\n\\end{equation}"
    "\\begin{equation*}\n|\n\\end{equation*}"
    "\\begin{align}\\label{|}\n\n\\end{align}"
    "\\begin{align*}\n|\n\\end{align*}"
    "\\begin{gather}\\label{|}\n\n\\end{gather}"
    "\\begin{gather*}\n|\n\\end{gather*}"
    "\\begin{multline}\\label{|}\n\n\\end{multline}"
    "\\begin{multline*}\n|\n\\end{multline*}")
  "List of strings inserted by hitting $.
The first time you hit $, the first string is inserted.  Hitting it
again replaces the first string with the second, etc.  After inserting
each string, the cursor is placed at the spot marked by the string
ultex-dollar-array-cursor (which has default value `|').  If you do
not include the string ultex-dollar-array-cursor anywhere, Emacs will
try to guess where to put the cursor.  You may need to save the
setting of this variable and click `Hide' and then `Show' to display
the guessed cursor position."
  :type '(repeat string)
  :tag "Ultex Dollar Array"
  :set (lambda (symbol value)
	 (setq ultex-dollar-array (ultex-convert-dollar-array-external
				   value))
	 (set symbol (mapcar 'ultex-dollar-guess-cursor
		      value)))
  :group 'ultra-tex-environments)

(defvar ultex-dollar-array
  (ultex-convert-dollar-array-external
   ultex-dollar-array-external)
  "Array of strings inserted by hitting $.
The first time you hit $, the first string is inserted.  Hitting it
again replaces the first string with the second, etc.  Each entry is a
pair (string, number)--the string is inserted, and the point is then
moved forward the given number of spaces.")

(defun ultex-dollar-max ()
  "Length of ultex-dollar-array"
  (length ultex-dollar-array))

(defcustom ultex-dollar-back 1
  "Which element of ultex-dollar-array to start over with
once the elements have been cycled through once.  The elements are
numbered starting with 0."
  :type '(restricted-sexp :match-alternatives
			  (ultex-dollar-indexp))
  :group 'ultra-tex-environments)

(defun ultex-dollar-indexp (arg)
  (and (integerp arg)
       (<= 0 arg)
       (<= arg (ultex-dollar-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ultra-tex-misc
;;

(defcustom ultex-slash-dollar-is-not-math t
  "If non-nil, then typing `$' right after `\\' does not start math mode"
  :type '(boolean)
  :group 'ultra-tex-misc)

(defcustom ultex-dots-behavior 'dots
  "What happens when you insert three periods:
  'ams' means '\\dotsc' or '\\dotsb' is inserted, whichever seems
appropriate depending on the context: '\\dotsc' is inserted following
a comma, '\\dotsb' is inserted otherwise.
  'dots' means '\\dots' is inserted, regardless of the context.
  'latex' means '\\ldots' or '\\cdots' is inserted, whichever seems
appropriate.
  'nil' means '...' is inserted.

See also the variable 'ultex-dots-check-styles'."
  :type '(choice (const :tag "ams" ams)
		 (const :tag "dots" dots)
		 (const :tag "latex" latex)
		 (const :tag "nil" nil))
  :group 'ultra-tex-misc)

(make-variable-buffer-local 'ultex-dots-behavior)

(defcustom ultex-dots-check-styles t
  "Toggle whether to determine dots behavior from the documentclass, etc.
If non-nil, if the current document seems to use an AMS documentclass,
documentstyle, or package, then set ultex-dots-behavior to 'ams'."
  :type '(boolean)
  :group 'ultra-tex-misc)

(defcustom ultex-subscript-delete-trailing-space t
  "Toggle whether inserting a subscript _{}, superscript ^{}, or
hitting RET to move outside of braces or math mode, deletes trailing
spaces."
  :type '(boolean)
  :group 'ultra-tex-misc)

(defcustom ultex-dont-use-completion nil
  "*If non-nil, don't use lightning completion--use TAB to complete."
  :type '(boolean)
  :group 'ultra-tex-misc)

(defcustom ultex-display-pictures (or
				   (and
				    (boundp 'latex-symbol-picture-type)
				    latex-symbol-picture-type)
				   (featurep 'xpm))
  "Toggle display of pictures in greek keyboard.
Only relevant if using XEmacs which has been compiled with support for
xpm or some other picture format."
  :type '(boolean)
  :set (lambda (symbol value)
	 (setq ultex-display-pix-internal
	       (and value
		    (or (featurep 'xpm)
			(featurep 'gif)
			(featurep 'png)
			(featurep 'tiff)
			(featurep 'xface)
			(featurep 'jpeg)
			(featurep 'xbm))))
	 (set symbol value))
  :group 'ultra-tex-misc)

(defvar ultex-display-pix-internal ultex-display-pictures
  "Toggle display of pictures in greek keyboard.  Don't set by hand--
this gets set automatically when customizing ultex-display-pictures.")

(defcustom ultex-picture-type (if (boundp 'latex-symbol-picture-type)
				  latex-symbol-picture-type
				(if (featurep 'xpm) 'xpm
				  nil))
  "Format of picture file used."
  :type '(choice (const :tag "xpm" xpm)
		 (const :tag "png" png)
		 (const :tag "tiff" tiff)
		 (const :tag "xface" xface)
		 (const :tag "jpeg" jpeg)
		 (const :tag "xbm" xbm)
		 (const :tag "none" nil))
  :set (lambda (symbol value)
	 (setq ultex-picture-type-internal
	       (if (featurep value) value nil))
	 (set symbol value))
  :group 'ultra-tex-misc)

(defvar ultex-picture-type-internal ultex-picture-type
  "Format of picture file used.  Don't set by hand--this gets set
automatically when customizing ultex-picture-type.")

(defcustom ultex-picture-directory
  (if (locate-library "latex-symbols")
      (concat (substring (locate-library "latex-symbols")
			 0
			 (string-match "latex-symbols.el"
				       (locate-library "latex-symbols"))))
    (concat (substring ultex-guess-directory 0 -5)
	    "symbols/"))
  "Directory containing (La)TeX symbols.
This directory should have a subdirectory called \"xpm\" (or \"gif\" or
\"jpg\" or whatever, deppending on the value of `ultex-picture-type'."
  :type '(file :must-match t)
  :group 'ultra-tex-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; borrowed from aucify.el (and originally from the AUC-TeX file tex.el):
;;

(defcustom TeX-master t
  "The master file associated with the current buffer.
If the file being edited is actually included from another file, you
can tell AUC TeX the name of the master file by setting this variable.
If there are multiple levels of nesting, specify the top level file. 

If this variable is nil, AUC TeX will query you for the name.

If the variable is t, AUC TeX will assume the file is a master file
itself.

If the variable is 'shared, AUC TeX will query for the name, but not
change the file.  

It is suggested that you use the File Variables (see the info node in
the Emacs manual) to set this variable permanently for each file."
  :group 'TeX-command
  :group 'TeX-parse
  :group 'ultra-tex-auctex-files
  :type '(choice (const :tag "Query" nil)
		 (const :tag "This file" t)
		 (const :tag "Shared" shared)
		 (string :format "%v")))
(make-variable-buffer-local 'TeX-master)

;; set variables correctly if not using a new version of custom.
(if (not lc-custom-p)
    (progn
      (if (and ultex-use-color window-system (x-display-color-p))
	  (add-hook 'ultra-tex-mode-internal-hook
		    'turn-on-font-lock))
      (if ultex-use-font-latex
	  (progn
	    (require 'font-lock)
	    (require 'font-latex)
	    (setq font-latex-keywords font-latex-keywords-2)
	    (if lc-xemacs-p
		(put 'ultra-tex-mode 'font-lock-keywords
		     'font-latex-keywords)
	      (setq font-lock-defaults-alist
		    (cons (cons 'ultra-tex-mode
				'(font-latex-keywords
				  nil nil ((?$ . "\""))))
			  font-lock-defaults-alist))))
	(or lc-xemacs-p
	    (require 'font-lock)
	    (setq font-lock-defaults-alist
		  (cons (cons
			 'ultra-tex-mode
			 '((tex-font-lock-keywords
			    tex-font-lock-keywords-1
			    tex-font-lock-keywords-2)
			   nil nil ((?$ . "\"")) nil
			   (font-lock-mark-block-function
			    . mark-paragraph)))
			font-lock-defaults-alist))))
      (if ultex-use-auctex
	  (let ((file (locate-library "tex-buf")))
	    (if (stringp file)
		(progn
		  (defvar TeX-lisp-directory
		    (file-name-directory file)
		    "*The directory where the AUC TeX lisp files are located.")
		  (add-hook 'ultra-tex-mode-internal-hook
			    'ultra-tex-require-aucify))
	      (remove-hook 'ultra-tex-mode-internal-hook
			   'ultra-tex-require-aucify))))
      (if ultex-use-imenu
	  (if window-system
	      (progn
		(require 'imenu)
		(if lc-xemacs-p
		    (global-set-key [(shift button3)] 'imenu)
		  (global-set-key [S-mouse-3] 'imenu))
		(add-hook 'ultra-tex-mode-internal-hook
			  'ultra-tex-add-imenu))))
      (if ultex-use-bib-cite
	  (progn 
	    (require 'bib-cite)
	    (remove-hook 'ultra-tex-mode-internal-hook
			 'ultra-tex-add-imenu)
	    (add-hook 'ultra-tex-mode-internal-hook
		      'ultra-tex-add-bib-cite)
	    (if (and window-system ultex-use-imenu)
		(add-hook 'ultra-tex-mode-internal-hook
			  'ultra-tex-add-bib-cite-imenu))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ultex-cus)

;;; ultex-cus.el ends here
