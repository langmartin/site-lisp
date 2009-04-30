(defun fold (proc nil-value lst0)
  "Traditional fold-left, iterative style since we don't have tail calls."
  (let ((acc nil-value)
        (lst lst0))
    (while lst
      (setq acc
            (funcall proc (car lst) acc))
      (setq lst (cdr lst)))
    acc))

(assert (eql 6 (fold '+ 0 '(1 2 3)))
        (let* ((test '(1 2 3))
               (ref test))
          (fold '+0 ref)
          (eq ref test)))

(defun foldr (proc nil-value lst)
  "Traditional fold-right, will blow up the stack"
  (if (null lst)
      nil-value
    (funcall proc
             (car lst)
             (foldr proc nil-value (cdr lst)))))

(assert (eql 6 (foldr '+ 0 '(1 2 3)))
        (let* ((test '(1 2 3))
               (ref test))
          (foldr '+0 ref)
          (eq ref test)))

(defun intersperse (lst el)
  (cons (car lst)
        (foldr (lambda (x acc)
                 (cons el
                       (cons x
                             acc)))
               '()
               (cdr lst))))

(assert (equal (intersperse '(1 2 3) 8)
               '(1 8 2 8 3)))

(provide 'srfi-1)
