(defmacro and-let1 (binding &rest body)
  "Bind one pair, and execute the body if the value of
the binding is true. Use and-let* instead."
  (declare (indent 1))
  (let ((sym (car binding))
        (exp (cdr binding)))
    (if (null exp)
        `(if ,sym (progn ,@body))
      `(let ((,sym ,@exp))
         (if ,sym (progn ,@body))))))

(defmacro and-let* (bindings &rest body)
  "Bind variables like let*, but ensuring that each value is true
  in sequences. As values are true, continue to bind and then
  execute the body. See scheme srfi-2."
  (declare (indent 1))
  (if (null bindings)
      `(progn ,@body)
    `(and-let1 ,(car bindings)
       (and-let* ,(cdr bindings)
         ,@body))))

(assert
 (and
  (= 3 (and-let* ((foo 1) (bar 2)) (+ foo bar)))
  (null (and-let* ((foo nil) (error "shouldn't reach this line"))))
  (eq 'a (and-let* ((foo (memq 'a '(b c a d)))
                    ((listp foo))
                    (foo (car foo)))
           foo))))

(provide 'srfi-2)
