;;; aucify.el --- Stuff to make Ultra-TeX compatible with AUC-TeX
;; Copyright (c) 1996, 1997, 1998, 1999 Mark Hovey, John Palmieri

;; Authors:   Mark Hovey <hovey@member.ams.org>, 
;;            John Palmieri <palmieri@math.washington.edu>
;; Keywords: AUC-TeX vs. Ultra-TeX
;; Version:  0.63 of Tue Jun 29 14:39:57 PDT 1999

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: 
;;   
;; This file defines variables and keys in ultra-tex-mode-map which
;; let Ultra-TeX mode use the AUC-TeX functions (from the file
;; tex-buf.el) that run (La)TeX on your buffer, start up a viewer,
;; print the file, etc.  See the AUC-TeX documentation for how to use
;; this (basically, you just hit C-c C-c).
;;
;; The contents of this file are modifications of the parts of the
;; AUC-TeX package.  See
;;
;;      http://sunsite.auc.dk/auctex/
;;
;; for the current version of AUC-TeX.
;;

(defcustom ultex-ignore-auctex-tex.el t
  "If non-nil, the AUC-TeX file tex.el will not be loaded when `required'."
  :type '(boolean)
  :group 'ultra-tex-auctex)

(if ultex-ignore-auctex-tex.el (provide 'tex))

(require 'tex-buf)
(require 'ultex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Customization
;;

(defgroup ultra-tex-auctex nil
  "Options for using AUC-TeX commands with Ultra-TeX mode."
  :tag "Ultra-TeX and AUC-TeX"
  :prefix "ultex"
  :link '(custom-manual "(ultra)Top")
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'ultra-tex)

(defgroup ultra-tex-auctex-files nil
  "File names and paths in Ultra-TeX/AUC-TeX."
  :group 'ultra-tex-auctex)
  
(defgroup ultra-tex-auctex-commands nil
  "External commands in Ultra-TeX via AUC-TeX."
  :group 'ultra-tex-auctex)
  
(defgroup ultra-tex-auctex-latex nil
  "LaTex stuff for Ultra-TeX/AUC-TeX."
  :group 'ultra-tex-auctex)
  
(if (null ultra-tex-mode-map)
    (progn
      (ultex-define-ultra-tex-mode-map)
      (ultex-reset-greek-keys)))

(define-key ultra-tex-mode-map "\C-c\C-w" 'TeX-toggle-debug-boxes)
(define-key ultra-tex-mode-map "\C-c`" 'TeX-next-error)
(define-key ultra-tex-mode-map "\C-c\C-l" 'TeX-recenter-output-buffer) 
(define-key ultra-tex-mode-map "\C-c\C-k" 'TeX-kill-job)
(define-key ultra-tex-mode-map "\C-c\C-c" 'TeX-command-master)
(define-key ultra-tex-mode-map "\C-c\C-t" 'TeX-command-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group: ultra-tex-auctex
;;

(defcustom TeX-display-help t
  "*Non-nil means popup help when stepping thrugh errors with
\\[TeX-next-error]"
  :group 'TeX-output
  :group 'ultra-tex-auctex
  :type 'boolean)

(defcustom TeX-debug-bad-boxes nil
  "*Non-nil means also find overfull/underfull boxes warnings with
TeX-next-error"
  :group 'TeX-output
  :group 'ultra-tex-auctex
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group: ultra-tex-auctex-latex
;;

(defcustom LaTeX-version "2e"
  "Default LaTeX version.  Currently recognized is \"2\" and \"2e\"."
  :group 'LaTeX
  :group 'ultra-tex-auctex-latex
  :type '(radio (const :format "%v\n%h"
		       :doc "\
The executable `latex' is LaTeX version 2."
		       "2")
		(const :format "%v\n%h"
		       :doc "\
The executable `latex' is LaTeX version 2e.
Do *not* select this if you need to run `latex2e' in order to get
LaTeX version 2e."
		       "2e")
		(string :tag "Other")))
		
(defcustom LaTeX-command-style
  (if (string-equal LaTeX-version "2")
      ;; There is a lot of different LaTeX 2 based formats.
      '(("^latex2e$" "latex2e")
	("^foils$" "foiltex")
	("^ams" "amslatex")
	("^slides$" "slitex")
	("^plfonts\\|plhb$" "platex")
	("." "latex"))
    ;; They have all been combined in LaTeX 2e.
    '(("." "latex")))
  "List of style options and LaTeX commands.

If the first element (a regular expresion) matches the name of one of
the style files, any occurrence of the string %l in a command in
TeX-command-list will be replaced with the second element.  The first
match is used, if no match is found the %l is replaced with the empty
string."
  :group 'TeX-command
  :group 'ultra-tex-auctex-latex
  :type '(repeat (group :value ("" "")
			regexp (string :tag "Style"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group: ultra-tex-auctex-files
;;

;; Master File
(defcustom TeX-master t
  "*The master file associated with the current buffer.
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

(defcustom TeX-one-master "\\.tex$"
  "*Regular expression matching ordinary TeX files.

You should set this variable to match the name of all files, where
automatically adding a file variable with the name of the master file
is a good idea.  When AUC TeX add the name of the master file as a
file variable, it does not need to ask next time you edit the file.  

If you dislike AUC TeX automatically modifying your files, you can set
this variable to \"<none>\"."
  :group 'TeX-command
  :group 'ultra-tex-auctex-files
  :type 'regexp)

(defvar TeX-convert-master t
  "*If not nil, automatically convert ``Master:'' lines to file variables.
This will be done when AUC TeX first try to use the master file.")

(defcustom TeX-file-extensions '("tex" "sty" "cls" "ltx" "texi" "texinfo")
  "*File extensions used by manually generated TeX files."
  :group 'TeX-file-extension
  :group 'ultra-tex-auctex-files
  :type '(repeat (string :format "%v")))

(defcustom TeX-macro-global '("/usr/local/lib/texmf/tex/")
  "Directories containing the sites TeX macro files and style files.
The directory names *must* end with a slash."
  :group 'TeX-file
  :group 'ultra-tex-auctex-files
  :type '(repeat (directory :format "%v")))

(defun ultex-add-trailing-slash (str)
  "If STR ends in /, return STR.  Otherwise, return STR with /
concatenated."
  (if (string= (substring str -1) "/")
      str
    (concat str "/")))
  
;; definition modified by John Palmieri to use ultex-parse-colon-path
;; instead of TeX-parse-path
(defcustom TeX-macro-private
  (mapcar 'ultex-add-trailing-slash
	  (append (ultex-parse-colon-path
		   (getenv "TEXINPUTS"))
		  (ultex-parse-colon-path
		   (getenv "BIBINPUTS"))))
  "Directories where you store your personal TeX macros.
Each must end with a slash."
  :group 'TeX-file
  :group 'ultra-tex-auctex-files
  :type '(repeat (file :format "%v")))

(defcustom TeX-check-path
  (append (list "./") TeX-macro-private TeX-macro-global)
  "Directory path to search for dependencies.

If nil, just check the current file.
Used when checking if any files have changed."
  :group 'TeX-file
  :group 'ultra-tex-auctex-files
  :type '(repeat (file :format "%v")))

(defcustom BibTeX-file-extensions '("bib")
  "Valid file extensions for BibTeX files."
  :group 'TeX-file-extension
  :group 'ultra-tex-auctex-files
  :type '(repeat (string :format "%v")))

(defcustom BibTeX-style-extensions '("bst")
  "Valid file extensions for BibTeX styles."
  :group 'TeX-file-extension
  :group 'ultra-tex-auctex-files
  :type '(repeat (string :format "%v")))

(defcustom TeX-default-extension "tex"
  "*Default extension for TeX files."
  :group 'TeX-file-extension
  :group 'ultra-tex-auctex-files
  :type 'string)

(make-variable-buffer-local 'TeX-default-extension)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group: ultra-tex-auctex-commands
;;

(defcustom TeX-command-BibTeX "BibTeX"
  "*The name of the BibTeX entry in TeX-command-list."
  :group 'TeX-command-name
  :group 'ultra-tex-auctex-commands
  :type 'string)
(make-variable-buffer-local 'TeX-command-BibTeX)

(defcustom TeX-command-Show "View"
  "*The default command to show (view or print) a TeX file.
Must be the car of an entry in TeX-command-list."
  :group 'TeX-command-name
  :group 'ultra-tex-auctex-commands
  :type 'string)
(make-variable-buffer-local 'TeX-command-Show)

(defcustom TeX-command-Print "Print"
  "The name of the Print entry in TeX-command-Print."
  :group 'TeX-command-name
  :group 'ultra-tex-auctex-commands
  :type 'string)

(defcustom TeX-command-Queue "Queue"
  "The name of the Queue entry in TeX-command-Queue."
  :group 'TeX-command-name
  :group 'ultra-tex-auctex-commands
  :type 'string)

(defcustom TeX-print-command "dvips %s -P%p"
  "*Command used to print a file. 

First %p is expanded to the printer name, then ordinary expansion is
performed as specified in TeX-expand-list."
  :group 'TeX-command
  :group 'ultra-tex-auctex-commands
  :type 'string)

(defcustom TeX-queue-command "lpq -P%p"
  "*Command used to show the status of a printer queue. 

First %p is expanded to the printer name, then ordinary expansion is
performed as specified in TeX-expand-list."
  :group 'TeX-command
  :group 'ultra-tex-auctex-commands
  :type 'string)

(defcustom TeX-printer-list
  '(("Local" "dvips -f %s | lpr" "lpq")
    ("lw") ("ps"))
  "List of available printers.

The first element of each entry is the printer name.

The second element is the command used to print to this
printer.  It defaults to the value of TeX-print-command.

The third element is the command used to examine the print queue for
this printer.  It defaults to the value of TeX-queue-command.

Any occurence of `%p' in the second or third element is expanded to
the printer name given in the first element, then ordinary expansion
is performed as specified in TeX-expand-list."
  :group 'TeX-command
  :group 'ultra-tex-auctex-commands
  :type '(repeat (group (string :tag "Name")
			(option (group :inline t
				       :extra-offset -4
				       (choice :tag "Print"
					       (const :tag "default")
					       (string :format "%v"))
				       (option (choice :tag "Queue"
						       (const :tag "default")
						       (string 
							:format "%v"))))))))

(defcustom TeX-printer-default (or (getenv "PRINTER")
				   (and TeX-printer-list
					(car (car TeX-printer-list)))
				   "lw")
  "*Default printer to use with TeX-command."
  :group 'TeX-command
  :group 'ultra-tex-auctex-commands
  :type 'string)

(defcustom TeX-view-style '(("^a5$" "xdvi %d -paper a5")
			    ("^landscape$" "xdvi %d -paper a4r -s 4")
			    ;; The latest xdvi can show embedded postscript.
			    ;; If you don't have that, uncomment next line.
			    ;; ("^epsf$" "ghostview %f")
			    ("." "xdvi %d"))
  "List of style options and view options.

If the first element (a regular expresion) matches the name of one of
the style files, any occurrence of the string %v in a command in
TeX-command-list will be replaced with the second element.  The first
match is used, if no match is found the %v is replaced with the empty
string."
  :group 'TeX-command
  :group 'ultra-tex-auctex-commands
  :type '(repeat (group regexp (string :tag "Command"))))

(defcustom TeX-expand-list 
  (list (list "%p" 'TeX-printer-query)	;%p must be the first entry
	(list "%q" (function (lambda ()
		     (TeX-printer-query TeX-queue-command 2))))
	(list "%v" 'TeX-style-check TeX-view-style)
	(list "%l" 'TeX-style-check LaTeX-command-style)
	(list "%s" 'file nil t)
	(list "%t" 'file 't t)
	(list "%d" 'file "dvi" t)
	(list "%f" 'file "ps" t))
  "List of expansion strings for TeX command names.

Each entry is a list with two or more elements.  The first element is
the string to be expanded.  The second element is the name of a
function returning the expanded string when called with the remaining
elements as arguments.  The special value `file' will be expanded to
the name of the file being processed, with an optional extension."
  :group 'TeX-command
  :group 'ultra-tex-auctex-commands
  :type '(repeat (group (string :tag "Key")
			(sexp :tag "Expander")
			(repeat :inline t
				:tag "Arguments"
				(sexp :format "%v")))))

(defcustom TeX-command-list
  ;; You may have to remove the single quotes around the command
  ;; arguments if you use DOS.
  (list (list "TeX" "tex '\\nonstopmode\\input %t'" 'TeX-run-TeX nil t)
	(list "TeX Interactive" "tex %t" 'TeX-run-interactive nil t)
	(list "LaTeX" "%l '\\nonstopmode\\input{%t}'"
	      'TeX-run-LaTeX nil t)
	(list "LaTeX Interactive" "%l %t" 'TeX-run-interactive nil t)
	(list "LaTeX2e" "latex2e '\\nonstopmode\\input{%t}'"
	      'TeX-run-LaTeX nil t)
	(if (or window-system (getenv "DISPLAY"))
	    ;; suggestion: customize to turn off the t toggle
	    (list "View" "%v " 'TeX-run-background t nil)
	  (list "View" "dvi2tty -q -w 132 %s " 'TeX-run-command t nil))
	(list "Print" "%p " 'TeX-run-command t nil)
	(list "Queue" "%q" 'TeX-run-background nil nil)
	(list "File" "dvips %d -o %f " 'TeX-run-command t nil)
	(list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil)
	(list "Index" "makeindex %s" 'TeX-run-command nil t)
	;; (list "Check" "chktex -v3 %s" 'TeX-run-compile nil t)
	;; Uncomment the above line and comment out the next line to
	;; use `chktex' instead of `lacheck'. 
	(list "Check" "lacheck %s" 'TeX-run-compile nil t)
	(list "Spell" "<ignored>" 'TeX-run-ispell nil nil)
	(list "Other" "" 'TeX-run-command t t)
	;; Not part of standard TeX.
	(list "Makeinfo" "makeinfo %t" 'TeX-run-compile nil t)
	(list "AmSTeX" "amstex '\\nonstopmode\\input %t'"
	      'TeX-run-TeX nil t))
  "List of commands to execute on the current document.

Each element is a list, whose first element is the name of the command
as it will be presented to the user.  

The second element is the string handed to the shell after being
expanded. The expansion is done using the information found in
TeX-expand-list. 

The third element is the function which actually start the process.
Several such hooks has been defined:

TeX-run-command: Start up the process and show the output in a
separate buffer.  Check that there is not two commands running for the
same file.  Return the process object. 

TeX-run-format: As TeX-run-command, but assume the output is created
by a TeX macro package.  Return the process object. 

TeX-run-TeX: For TeX output.

TeX-run-LaTeX: For LaTeX output.

TeX-run-interactive: Run TeX or LaTeX interactively.

TeX-run-BibTeX: For BibTeX output.

TeX-run-compile: Use `compile' to run the process.  

TeX-run-shell: Use `shell-command' to run the process.

TeX-run-discard: Start the process in the background, discarding its
output.

TeX-run-background: Start the process in the background, show output
in other window.

TeX-run-dviout: Special hook for the Japanese dviout previewer for
PC-9801.

To create your own hook, define a function taking three arguments: The
name of the command, the command string, and the name of the file to
process.  It might be useful to use TeX-run-command in order to
create an asynchronous process.

If the fourth element is non-nil, the user will get a chance to
modify the expanded string.

The fifth element is obsolete and ignored."
  :group 'ultra-tex-auctex-commands
  :type '(repeat (group (string :tag "Name")
			(string :tag "Command")
			(choice :tag "How"
				:value TeX-run-command
				(function-item TeX-run-command)
				(function-item TeX-run-format)
				(function-item TeX-run-TeX)
				(function-item TeX-run-LaTeX)
				(function-item TeX-run-interactive)
				(function-item TeX-run-BibTeX)
				(function-item TeX-run-compile)
				(function-item TeX-run-shell)
				(function-item TeX-run-discard)
				(function-item TeX-run-background)
				(function-item TeX-run-dviout)
				(function :tag "Other"))
			(boolean :tag "Prompt")
			(sexp :format "End\n"))))
		       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other variables
;;

;; stuff for TeX-command-region

(defvar TeX-esc "\\" "The TeX escape character.")
(make-variable-buffer-local 'TeX-esc)

(defvar TeX-grop "{" "The TeX group opening character.")
(make-variable-buffer-local 'TeX-grop)

(defvar TeX-grcl "}" "The TeX group closing character.")
(make-variable-buffer-local 'TeX-grcl)

(defvar LaTeX-trailer-start
  (concat (regexp-quote TeX-esc) "end *" TeX-grop "document" TeX-grcl)
  "Default start of trailer marker for LaTeX documents.")

(defvar TeX-trailer-start LaTeX-trailer-start
  "Regular expression delimiting start of trailer in a TeX file.")

(make-variable-buffer-local 'TeX-trailer-start)

(defvar LaTeX-header-end
  (concat (regexp-quote TeX-esc) "begin *" TeX-grop "document" TeX-grcl)
  "Default end of header marker for LaTeX documents.")

(defvar TeX-header-end LaTeX-header-end
  "Regular expression delimiting end of header in a TeX file.")

(make-variable-buffer-local 'TeX-header-end)

(defvar TeX-command-current 'TeX-command-master)
;; Function used to run external command.

(defvar TeX-command-force nil)
;; If non-nil, TeX-command-query will return the value of this
;; variable instead of quering the user. 

(defvar TeX-command-default ""
  "The default command for TeX-command in the current major mode.")

(make-variable-buffer-local 'TeX-command-default)

(defvar TeX-auto-parser '((styles TeX-auto-file TeX-run-style-hooks)))
;; Alist of parsed information.  
;; Each entry is a list with the following elements:
;; 
;; 0. Name of information type.
;; 1. Name of temporary variable used when parsing.
;; 2. Name of function to add information to add to #3.
;; 3. Name of variable holding buffer local information.
;; 4. Name of variable indicating that #3 has changed.

(defconst TeX-auto-parser-temporary 1)
(defconst TeX-auto-parser-add 2)
(defconst TeX-auto-parser-local 3)
(defconst TeX-auto-parser-change 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;

(defun TeX-master-file (&optional extension nondirectory)
  "Return the name of the master file for the current document.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value `t' means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

Currently is will check for the presence of a ``Master:'' line in
the beginning of the file, but that feature will be phased out."
  (if (eq extension t)
      (setq extension TeX-default-extension))
  (let ((my-name (if (buffer-file-name)
                     (TeX-strip-extension nil (list TeX-default-extension) t)
                   "<none>")))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ;; Special value 't means it is own master (a free file).
	 ((equal TeX-master my-name) 
	  (setq TeX-master t))

	 ;; For files shared between many documents.
	 ((eq 'shared TeX-master)
	  (setq TeX-master
		(TeX-strip-extension
		 (read-file-name "Master file: (default this file) "
				 nil "///")
		 (list TeX-default-extension)
		 'path))
	  (if (or (string-equal TeX-master "///")
		  (string-equal TeX-master ""))
	      (setq TeX-master t)))

	 ;; We might already know the name.
	 (TeX-master)

	 ;; Support the ``Master:'' line (under protest!)
	 ((re-search-forward
	   "^%% *[Mm]aster:?[ \t]*\\([^ \t\n]+\\)" 500 t)
	  (setq TeX-master
		(TeX-strip-extension (TeX-match-buffer 1)
				     (list TeX-default-extension)))
	  (if TeX-convert-master
	      (progn
		(beginning-of-line)
		(kill-line 1)
		(TeX-add-local-master))))

	 ;; Is this a master file?
	 ((re-search-forward TeX-header-end 10000 t)
	  (setq TeX-master my-name))

	 ;; Ask the user (but add it as a local variable).
	 (t
	  (setq TeX-master
		(TeX-strip-extension
		 (condition-case name
		     (read-file-name "Master file: (default this file) "
				     nil "<default>")
		   (quit "<quit>"))
		 (list TeX-default-extension)
		 'path))
	  (cond ((string-equal TeX-master "<quit>")
		 (setq TeX-master t))
		((or (string-equal TeX-master "<default>")
		     (string-equal TeX-master ""))
		 (setq TeX-master t)
		 (TeX-add-local-master))
		(t
		 (TeX-add-local-master)))))))
  
    (let ((name (if (eq TeX-master t)
		    my-name
		  TeX-master)))
      
      (if (TeX-match-extension name)
      ;; If it already have an extension...
	  (if (equal extension TeX-default-extension)
	      ;; Use instead of the default extension
	      (setq extension nil)
	    ;; Otherwise drop it.
	    (setq name (TeX-strip-extension name))))
      
      ;; Remove directory if needed.
      (if nondirectory
	  (setq name (file-name-nondirectory name)))

      (if extension
	  (concat name "." extension)
	name))))

(defun TeX-add-local-master ()
  "Add local variable for TeX-master."

  (if (and (buffer-file-name)
           (string-match TeX-one-master
                         (file-name-nondirectory (buffer-file-name)))
           (not buffer-read-only))
      (progn
        (goto-char (point-max))
        (if (re-search-backward (concat "^\\([^\n]+\\)Local " "Variables:")
                                (- (point-max) 3000) t)
            (let ((prefix (TeX-match-buffer 1)))
              (re-search-forward (regexp-quote (concat prefix
                                                        "End:")))
              (beginning-of-line 1)
              (insert prefix "TeX-master: " (prin1-to-string TeX-master) "\n"))
          (insert "\n%%% Local " "Variables: \n"
                  "%%% mode: " (substring (symbol-name major-mode) 0 -5)
		  "\n"
                  "%%% TeX-master: " (prin1-to-string TeX-master) "\n"
                  "%%% End: \n")))))

(defun TeX-strip-extension (&optional string extensions nodir nostrip)
  "Return STRING without any trailing extension in EXTENSIONS.
If NODIR is `t', also remove directory part of STRING. 
If NODIR is `path', remove directory part of STRING if it is equal to
the current directory, TeX-macro-private or TeX-macro-global. 
If NOSTRIP is set, do not remove extension after all.
STRING defaults to the name of the current buffer.
EXTENSIONS defaults to TeX-file-extensions."
  
  (if (null string)
      (setq string (or (buffer-file-name) "<none>")))
  
  (if (null extensions)
      (setq extensions TeX-file-extensions))
  
  (let* ((strip (if (and (not nostrip)
			 (TeX-match-extension string extensions))
		    (substring string 0 (match-beginning 0))
		  string))
	 (dir (file-name-directory (expand-file-name strip))))
    (if (or (eq nodir t)
	    (string-equal dir (expand-file-name "./"))
	    (member dir TeX-macro-global)
	    (member dir TeX-macro-private))
        (file-name-nondirectory strip)
      strip)))

(defun TeX-match-extension (file &optional extensions)
  "Return non-nil if FILE has an one of EXTENSIONS.

If EXTENSIONS is not specified or nil, the value of
TeX-file-extensions is used instead."

  (if (null extensions)
      (setq extensions TeX-file-extensions))

  (let ((regexp (concat "\\.\\("
                        (mapconcat 'identity extensions "\\|")
                        "\\)$")))
    (string-match regexp file)))

(defun TeX-style-list ()
  "Return a list of all styles (subfils) use by the current document."
  '("dummy"))

(defun TeX-assoc (elem list)
  "Like assoc, except case incentive."
  (let ((case-fold-search t))
    (TeX-member elem list
		(function (lambda (a b)
		  (string-match (concat "^" (regexp-quote a) "$")
				(car b)))))))

(defun TeX-member (elt list how)
  "Returns the member ELT in LIST.  Comparison done with HOW.

Return nil if ELT is not a member of LIST."
  (while (and list (not (funcall how elt (car list))))
    (setq list (cdr list)))
  (car-safe list))

(defun TeX-function-p (arg)
  "Return non-nil if ARG is callable as a function."
  (or (and (fboundp 'byte-code-function-p)
	   (byte-code-function-p arg))
      (and (listp arg)
	   (eq (car arg) 'lambda))
      (and (symbolp arg)
	   (fboundp arg))))

(defun TeX-master-directory ()
  "Directory of master file."
  (abbreviate-file-name
   (expand-file-name
    (concat (file-name-directory buffer-file-name)
	    (file-name-directory (TeX-master-file))))))

(defun TeX-mark-active ()
  ;; In FSF 19 mark-active indicates if mark is active.
  mark-active)

(defun TeX-match-buffer (n)
  "Return the substring corresponding to the N'th match.
See match-data for details."
  (if (match-beginning n)
      (let ((str (buffer-substring (match-beginning n) (match-end n))))
	(set-text-properties 0 (length str) nil str)
	(copy-sequence str))
    ""))

(or (fboundp 'LaTeX-bibliography-list)
    (defun LaTeX-bibliography-list nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu
;;   taken entirely from tex.el in the AUC-TeX distribution (except
;;   for the first three arguments in easy-menu-define).
;;

(defvar TeX-command-current 'TeX-command-master)
;; Function used to run external command.

(defun TeX-command-select-master ()
  (interactive)
  (message "Next command will be on the master file")
  (setq TeX-command-current 'TeX-command-master))

(defun TeX-command-select-buffer ()
  (interactive)
  (message "Next command will be on the buffer")
  (setq TeX-command-current 'TeX-command-buffer))

(defun TeX-command-select-region ()
  (interactive)
  (message "Next command will be on the region")
  (setq TeX-command-current 'TeX-command-region))

(defvar TeX-command-force nil)
;; If non-nil, TeX-command-query will return the value of this
;; variable instead of quering the user. 

(defun TeX-command-menu (name)
  ;; Execute TeX-command-list NAME from a menu.
  (let ((TeX-command-force name))
    (funcall TeX-command-current)))

(defun TeX-command-menu-print (printer command name)
  ;; Print on PRINTER using method COMMAND to run NAME.
  (let ((TeX-printer-default printer)
	(TeX-printer-list nil)
	(TeX-print-command command))
    (TeX-command-menu name)))

;; jhp: I don't know where lookup and command come from, but to avoid
;; byte-compiler warnings...

(defvar lookup nil "")
(defvar command nil "")

(defun TeX-command-menu-printer-entry (entry)
  ;; Return TeX-printer-list ENTRY as a menu item.
  (vector (nth 0 entry)
	  (list 'TeX-command-menu-print
		(nth 0 entry)
		(or (nth lookup entry) command)
		name)
	  t))

;; Begin fix part 1 by Ulrik Dickow <dickow@nbi.dk> 16-Feb-1996,
;; to make queue command usable.  Easy but ugly code duplication again.

(defun TeX-command-menu-queue (printer command name)
  ;; Show queue for PRINTER using method COMMAND to run NAME.
  (let ((TeX-printer-default printer)
	(TeX-printer-list nil)
	(TeX-queue-command command))
    (TeX-command-menu name)))

(defun TeX-command-menu-queue-entry (entry)
  ;; Return TeX-printer-list ENTRY as a menu item.
  (vector (nth 0 entry)
	  (list 'TeX-command-menu-queue
		(nth 0 entry)
		(or (nth lookup entry) command)
		name)
	  t))

(defun TeX-command-menu-entry (entry)
  ;; Return TeX-command-list ENTRY as a menu item.
  (let ((name (car entry)))
    (cond ((and (string-equal name TeX-command-Print)
		TeX-printer-list)
	   (let ((command TeX-print-command)
		 (lookup 1))
	     (append (list TeX-command-Print)
		     (mapcar 'TeX-command-menu-printer-entry
			     TeX-printer-list))))
	  ((and (string-equal name TeX-command-Queue)
		TeX-printer-list)
	   (let ((command TeX-queue-command)
		 (lookup 2))
	     (append (list TeX-command-Queue)
		     (mapcar 'TeX-command-menu-queue-entry ; dickow fix part 2.
			     TeX-printer-list))))
	  (t
	   (vector name (list 'TeX-command-menu name) t)))))

(easy-menu-define
 aucify-mode-menu
 ultra-tex-mode-map
 "TeX commands menu used in Ultra-TeX mode."
 (append '("TeX-commands")
	 '(("Command on"
	    [ "Master File" TeX-command-select-master
	      :keys "C-c C-c" :style radio
	      :selected (eq TeX-command-current 'TeX-command-master) ]
	    [ "Buffer" TeX-command-select-buffer
	      :keys "C-c C-b" :style radio
	      :selected (eq TeX-command-current 'TeX-command-buffer) ]
	    [ "Region" TeX-command-select-region
	      :keys "C-c C-r" :style radio
	      :selected (eq TeX-command-current 'TeX-command-region) ]))
	 (let ((file 'TeX-command-on-current))
	   (mapcar 'TeX-command-menu-entry TeX-command-list))))

(provide 'aucify)

;;; aucify.el ends here
