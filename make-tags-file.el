(require 'find-lisp)
(require 'srfi-2)

(defvar make-tags-file-patterns)
(setq make-tags-file-patterns
      '("\\.js$"
        "\\.asp$"
        "\\.el$"))

(defvar make-tags-file-anti-patterns)
(setq make-tags-file-anti-patterns
      '("\\.min\\.js$"))

(defvar make-tags-file-cvs-style '("CVS" "svn"))

(defvar make-tags-file-git-style '(".git" "_dargs" ".hg"))

(defvar make-tags-file-skip-directories)
(setq make-tags-file-skip-directories
      (append make-tags-file-cvs-style
              make-tags-file-git-style
              '(".attic")))

(defun find-root-directory (&optional procedure)
  "Find a (the) root directory, using the procedure as a predicate."
  (if (or (and procedure (funcall procedure))
          (= (nth 10 (file-attributes "."))
             (nth 10 (file-attributes ".."))))
      (file-truename ".")
    (progn
      (cd "..")
      (find-root-directory procedure))))

(defun generalized-member (compare element sequence)
  (let ((lst sequence)
        (result nil))
    (while lst
      (if (funcall compare element (car lst))
          (progn
            (setq result lst)
            (setq lst nil))
        (setq lst (cdr lst))))
    result))

(assert
 (equal (generalized-member 'eql 'a '(b c a d))
        '(a d)))

(defun match-regexp-list (str regexp-list)
  (and-let* ((match (generalized-member
                     (lambda (str re)
                       (string-match re str))
                     str
                     regexp-list)))
    (car match)))

(assert
 (equal "\\.el$"
        (match-regexp-list "foo.el" '("\\.el$"))))

(defun make-tags-file-cvsp ()
  (generalized-member
   (lambda (junk el) (file-exists-p el))
   nil
   make-tags-file-cvs-style))

(defun make-tags-file-gitp ()
  (generalized-member
   (lambda (junk el) (file-exists-p el))
   nil
   make-tags-file-git-style))

(defun make-tags-file ()
  "Make an etags file in the current project"
  (interactive)
  (make-tags-file-make
   (if (file-exists-p "CVS")
       (lambda () (not (file-exists-p "../CVS")))
     'make-tags-file-gitp)))

(defun make-tags-file-make (pred)
  (apply
   'call-process
   (append
    (list "etags" nil nil nil)
    (make-tags-file-list pred))))

(defun make-tags-file-filep (file dir)
  (if (file-directory-p file)
      nil
    (if (match-regexp-list file make-tags-file-anti-patterns)
        nil
      (match-regexp-list file make-tags-file-patterns))))

(defun make-tags-file-directoryp (dir parent)
  (if (generalized-member 'string= dir make-tags-file-skip-directories)
      nil
    (find-lisp-default-directory-predicate dir parent)))

(defun make-tags-file-list (pred)
  (let ((dir (find-root-directory pred)))
    (message "top directory here is %s" dir)
    (if (string-equal dir "/")
        (error "Can't find a project in this path")
      (progn
        (cd dir)
        (if (file-exists-p ".make-tags-file.el")
            (mapcar (lambda (x)
                      (if (symbolp x)
                          (symbol-name x)
                        x))
                    (with-input-file ".make-tags-file.el" 'read-all))
          (find-lisp-find-files-internal
           "."
           'make-tags-file-filep
           'make-tags-file-directoryp))))))

(provide 'make-tags-file)
