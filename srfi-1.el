(defun fold (proc nil-value seq)
  "The fundamental list iterator."
  (let ((acc nil-value))
    (mapc (lambda (el)
            (setq acc (funcall proc el acc)))
          seq)
    acc))

(assert (eql 6 (fold '+ 0 '(1 2 3)))
        (let* ((test '(1 2 3))
               (ref test))
          (fold '+0 ref)
          (eq ref test)))

(defun fold-right (proc nil-value lst)
  "The fundamental list recursion operator"
  (if (null lst)
      nil-value
    (funcall proc
             (car lst)
             (fold-right proc nil-value (cdr lst)))))

(assert (eql 6 (fold-right '+ 0 '(1 2 3)))
        (let* ((test '(1 2 3))
               (ref test))
          (fold-right '+0 ref)
          (eq ref test)))

(defun intersperse (lst el)
  (cons (car lst)
        (fold-right (lambda (x acc)
                      (cons el
                            (cons x
                                  acc)))
                    '()
                    (cdr lst))))

(assert (equal (intersperse '(1 2 3) 8)
               '(1 8 2 8 3)))


(defun filter (proc lst)
  "Like mapcar, but the new list is constructed using only the
true values returned by proc."
  (let ((value nil))
    (while lst
      (if (funcall proc (car lst))
          (setq value (cons (car lst) value)))
      (setq lst (cdr lst)))
    value))

(defun unfold (stopp val next seed tail-gen)
  (if (funcall stopp seed) (funcall tail-gen seed)
    (cons (val seed)
          (unfold stopp val next (funcall next seed) tail-gen))))

(provide 'srfi-1)
