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

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (set-variable 'comment-start nil 'make-local)))

(setq org-file-apps
      (cons '(directory . emacs)
            org-file-apps))

(set-variables
 '(nav-boring-file-regexps (quote ("\\.py[co]$" "\\.o$" "~$" "\\.bak$" "^\\.[^/]" "^\\./?$" "/\\." "\\.min\\.js$" "\\.elc$")))
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time)))

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

(provide 'rc-org-mode)

