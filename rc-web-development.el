(require 'visual-basic-mode)
(require 'sgml-mode)
(setq visual-basic-mode-indent 4)
(add-hook 'visual-basic-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(defun rc-sgml-mode-for-asp ()
  (interactive)
  (widen)
  (html-mode))

(add-to-auto-mode-alist
 '(("\\.asp$" . html-mode)
   ("\\.asa$" . visual-basic-mode)
   ("\\.vbs$" . visual-basic-mode)
   ("\\.vb$" . visual-basic-mode)
   ("\\.bas$" . visual-basic-mode)))

(add-hook 'sgml-mode-hook
	  (lambda ()
            (local-set-keys
             '(("C-c C-p" . sgml-skip-tag-backward)
               ("C-c C-n" . sgml-skip-tag-forward)))))

(define-key sgml-mode-map "\C-x\C-s" 'cleanup-untabify-save)

(defun enable-truncate-long-lines ()
  (toggle-truncate-lines 1))

(add-hook 'html-mode-hook 'enable-truncate-long-lines)

(put 'narrow-to-region 'disabled nil)

(require 'html-script)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;;; http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(provide 'rc-web-development)
