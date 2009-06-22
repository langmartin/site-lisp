(require 'utility)

(defun expression-in-file (file symbol)
  (generalized-member
   (lambda (arg elt)
     (eq arg (car elt)))
   symbol
   (with-input-file file 'read-all)))

(expression-in-file "~/.emacs" 'custom-set-variables)
