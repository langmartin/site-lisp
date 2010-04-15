(defmacro defproj (name path)
  `(progn
     (setq ,name ,path)
     (defun ,name ()
       (interactive)
       (save-default-directory
           ,name
         (make-tags-file))
       (find-file ,path)
       (visit-tags-table "TAGS"))))

(defproj proj-dds
  "//chadedmw1/apps/FileNET/IDM/Web/IDMWS/DDS-lmartin/")

(defproj proj-admin
  "//chadedmw1/apps/FileNET/IDM/Web/IDMWS/administration/")

(defproj proj-pc
  "//chadedmw1/apps/FileNET/IDM/Web/IDMWS_Int/pc-lmartin/")

(provide 'rc-proj)
