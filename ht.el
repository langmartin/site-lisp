(setq ht-identifier-component-table
  '((?# . "id")
    (?. . "class")
    (?$ . "name")
    (?: . "type")
    (?, . 'attr)
    (?\[ . 'attr)
    (?\] . nil)))

(defun ht-parse-identifier (id)
  (let ((state nil)
        (buffer ""))

    (defun ht-buffer (ch) (princ ch))
    
    (defun ht-switch! (new-state)
      (setq state new-state)
      (prog1
          buffer
        (setq buffer "")))
  
    (mapc (lambda (ch)
            (let* ((attr (memq ch ht-identifier-component-table))
                   (attr (and attr (cdr attr))))
              
              (if 
                  
                )
                
              )
            )))
  )
