(require 'htmlize)

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

(require 'org-collector)

(set-variables
 '(nav-boring-file-regexps (quote ("\\.py[co]$" "\\.o$" "~$" "\\.bak$" "^\\.[^/]" "^\\./?$" "/\\." "\\.min\\.js$" "\\.elc$")))
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time)))

(require 'org-export-latex)

;; (setq org-export-latex-classes
;;  (filter (lambda (pair)
;;            (if (equal (car pair) "langmartin")
;;                nil
;;              pair))
;;          org-export-latex-classes))

(add-to-alist/equal
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
"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
 'equal)

(provide 'rc-org-mode)

