(require 'utility)
(require 'srfi-1)
(require 'htmlize)
(require 'org)
(require 'org-attach)
(require 'org-collector)
(if (not (require 'org-export-latex nil t))
    (require 'org-latex))

(global-set-keys
 '(("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c ." . org-time-stamp)
   ("C-c C-x C-j" . org-clock-goto))
 'erc-mode-hook)

(defun org-kill-link-as-commit-message ()
  "Capture a stored link. Add it to the kill ring in a format suitable for use as a version control commit message."
  (interactive)
  (call-interactively 'org-store-link)
  (let ((link (car org-stored-links)))
    ;; A link is a (list URL DESCRIPTION)
    (kill-new (concat (cadr link) "\n" (car link) "\n"))
    (setq org-stored-links (cdr org-stored-links))))

(define-key org-mode-map (kbd "C-x C-s") 'cleanup-untabify-save)
(define-key org-mode-map (kbd "C-c M-l") 'org-kill-link-as-commit-message)

(progn
  (add-hook 'org-mode-hook 'comment-char-org)
  (add-hook 'org-mode-hook 'turn-off-tabs)
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(setq org-file-apps
      (cons '(directory . emacs)
            org-file-apps))

(custom-set-variables
 '(org-enforce-todo-dependencies t)
 '(org-log-done nil)
 '(org-clock-modeline-total (quote current))
 '(org-cycle-include-plain-lists nil)
 '(org-clock-into-drawer "LOGBOOK")
 '(org-adapt-indentation nil)
 '(org-hierarchical-checkbox-statistics nil)
 '(org-hierarchical-todo-statistics nil))

;;;; I've added the endnotes package to this header, it doesn't change
;;;; anything by default. In order to use it, you need to add two
;;;; literal latex includes to your org-mode file. At the top, add:
;;;;        #+LaTeX: \let\footnote=\endnote
;;;;
;;;; Then, wherever you want the notes to appear, add:
;;;;        #+LaTeX: \theendnotes

(update-alist
 'org-export-latex-classes
 '("langmartin"
  "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{amssymb}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage[colorlinks=true,pdfstartview=FitV,linkcolor=Blue,citecolor=Blue,urlcolor=Blue,filecolor=Blue]{hyperref}
\\usepackage{parskip}
\\setcounter{secnumdepth}{5}
\\usepackage{endnotes}

\\usepackage{titling}
\\pretitle{\\begin{center}\\LARGE}
\\posttitle{\\par\\end{center}\\vskip -2em}
\\preauthor{\\begin{center}
\\large \\lineskip 0em%
\\begin{tabular}[t]{c}}
\\postauthor{\\end{tabular}\\par\\end{center}}
\\predate{\\begin{center}\\large}
\\postdate{\\par\\end{center}}
"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
 'equal)

(custom-set-variables
 '(org-export-latex-default-class "langmartin"))

(defun org-shift-timestamps (start end n)
  "Update all timestamps in the region n hours"
  (interactive "r\nnAdd hours: ")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[[<]" end t)
      (when (org-at-timestamp-p t)
        (org-timestamp-change n 'hour)
        ))))

(defun rc-org-mode-line-less-decoration ()
  (require 'org-clock)
  (defun org-clock-get-clock-string ()
    "MONKEY PATCHED, the original is in org-clock.el. This one doesn't support special faces in the modeline, which makes active/inactive work as expected. The original doc string: Form a clock-string, that will be shown in the mode line. If an effort estimate was defined for the current item, use 01:30/01:50 format (clocked/estimated). If not, show simply the clocked time like 01:50."
    (let* ((clocked-time (org-clock-get-clocked-time))
           (h (floor clocked-time 60))
           (m (- clocked-time (* 60 h))))
      (if org-clock-effort
          (let* ((effort-in-minutes
                  (org-duration-string-to-minutes org-clock-effort))
                 (effort-h (floor effort-in-minutes 60))
                 (effort-m (- effort-in-minutes (* effort-h 60)))
                 (work-done-str
                  (org-propertize
                   (format org-time-clocksum-format h m)))
                 (effort-str (format org-time-clocksum-format effort-h effort-m))
                 (clockstr (org-propertize
                            (concat  "(%s/" effort-str
                                     ") "
                                     (replace-regexp-in-string "%" "%%" org-clock-heading)
                                     ))))
            (format clockstr work-done-str))
        (org-propertize (format
                         (concat "(" org-time-clocksum-format ") %s")
                         h m org-clock-heading))))))

(provide 'rc-org-mode)
