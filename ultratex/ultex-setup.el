;;; ultex-setup.el --- setup for Ultra-TeX mode
;; Copyright (c) 1996, 1997, 1998, 1999, 2000, 2001 Mark Haiman, Nick
;; Reingold, John Palmieri 

;; Authors:   Mark Haiman <mhaiman@macaulay.ucsd.edu>, 
;;            Nick Reingold, 
;;            John Palmieri <palmieri@math.washington.edu>
;; Maintainer: John Palmieri <palmieri@math.washington.edu>
;;             URL: http://www.math.washington.edu/~palmieri/Emacs/ultratex.html
;; Keywords: TeX-mode, completion
;; Version:  0.70 of Thu Oct 25 15:58:22 PDT 2001

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;
;; This file installs Ultra-TeX mode: it sets things up so Ultra-TeX
;; starts whenever you open a TeX file.

(defvar ultra-tex-mode-choice 'ultra-tex-mode
  "The mode you want to use with .tex files.
The default is ultra-tex-mode.  Another possible choice is latex-mode.")

(defvar ultra-tex-mode-alist-changes
  '(tex-mode latex-mode TeX-mode LaTeX-mode ultra-tex-mode)
  "A list of modes to change to ultra-tex-mode in auto-mode-alist.")

(defun ultra-tex-install (mode)
  "Change auto-mode-alist to use Ultra-TeX mode for suitable files.
Changes auto-mode-alist so that every mode name that appears in
ultra-tex-mode-alist-changes is replaced by ultra-tex-mode."
  (interactive "CMode: ")
  (let* ((ama (cons nil auto-mode-alist))
         (last ama)
         entry
	 newentry)
    (while (cdr last)
      (setq entry (car (cdr last)))
      (if (memq (cdr entry) ultra-tex-mode-alist-changes)
	  (progn
	    (setq newentry (cons (car entry) mode))
	    (setcdr last (cons newentry (cdr (cdr last))))))
      (setq last (cdr last)))
  (setq auto-mode-alist (cdr ama))))

(ultra-tex-install ultra-tex-mode-choice)

(autoload 'ultra-tex-mode "ultex" nil t)
(autoload 'tex-mode "tex" nil t)
(autoload 'TeX-mode "tex" nil t)
(autoload 'latex-mode "latex" nil t)
(autoload 'LaTex-mode "latex" nil t)

(provide 'ultex-setup)

;;; ultex-setup.el ends here
