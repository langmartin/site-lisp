;;;; Unstable
(defun rc-maybe-mmm-mode ()
  (interactive)
  (if (require 'mmm-auto "mmm-mode" t)
      (progn
        (setq mmm-global-mode 'maybe)
        (mmm-add-classes
         '((asp-javascript
            :submode javascript-mode
            :face mmm-code-submode-face
            :front "<%"
            :back "%>")
           (asp-vbscript
            :submode visual-basic-mode
            :face mmm-code-submode-face
            :front "<%"
            :back "%>")))
        (define-derived-mode asp-mode html-mode "ASP/HTML"))))

(defun rc-asp-javascript-mode ()
  "Broken still"
  (interactive)
  (rc-maybe-mmm-mode)
  (add-to-list 'auto-mode-alist '("\\.as[ap]\\'" . asp-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . asp-mode))
  (add-to-list 'mmm-mode-ext-classes-alist '(asp-mode nil html-js))
  (add-to-list 'mmm-mode-ext-classes-alist '(asp-mode nil embedded-css))
  (add-to-list 'mmm-mode-ext-classes-alist '(asp-mode nil asp-javascript)))
