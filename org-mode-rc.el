(require 'htmlize)

(global-set-keys
 '(("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c ." . org-time-stamp)))

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (set-variable 'comment-start nil 'make-local)))

(require 'org-collector)

(set-variables
 '(nav-boring-file-regexps (quote ("\\.py[co]$" "\\.o$" "~$" "\\.bak$" "^\\.[^/]" "^\\./?$" "/\\." "\\.min\\.js$" "\\.elc$")))
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time)))

(if (not (boundp 'org-export-latex-classes))
    (setq org-export-latex-classes nil))

(add-to-list
 'org-export-latex-classes
 '("langmartin"
  "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage[colorlinks=true,pdfstartview=FitV,linkcolor=blue,citecolor=blue,urlcolor=blue]{hyperref}
"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(provide 'org-mode-rc)

