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

(define-key org-mode-map "\C-x\C-s" 'cleanup-untabify-save)

(progn
  (add-hook 'org-mode-hook 'comment-char-org)
  (add-hook 'org-mode-hook 'turn-off-tabs)
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(setq org-file-apps
      (cons '(directory . emacs)
            org-file-apps))

(custom-set-variables
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-clock-modeline-total (quote current))
 '(org-cycle-include-plain-lists nil)
 '(org-clock-into-drawer 4)
 '(org-adapt-indentation nil))

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
\\usepackage[colorlinks=true,pdfstartview=FitV,linkcolor=blue,citecolor=blue,urlcolor=blue]{hyperref}
\\usepackage{parskip}
\\setcounter{secnumdepth}{5}
\\usepackage{endnotes}
"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
 'equal)

(defun org-shift-timestamps (start end n)
  "Update all timestamps in the region n hours"
  (interactive "r\nnAdd hours: ")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[[<]" end t)
      (when (org-at-timestamp-p t)
        (org-timestamp-change n 'hour)
        ))))

(provide 'rc-org-mode)
