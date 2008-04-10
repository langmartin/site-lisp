(defun fedcontent ()
  (interactive)
  (find-file "/jerk:/fedpub/data/current/content.tex"))

(defun fedcutline ()
  (interactive)
  (find-file "/jerk:/fedpub/data/current/cutline.txt"))

(defun s48manual ()
  (interactive)
  (w3m "file:///Users/lang/Sites/s48.org/1.7/manual/manual-Z-H-11.html#node_index_start"
       t)
  (rename-buffer "manual*"))

(defun s48riastradh ()
  (interactive)
  (w3m "file:///Users/lang/Sites/mumble.net/~campbell/s48-refman/html/index.html"
       t)
  (rename-buffer "riastradh*"))

(defun mysqlgrant (db user pass)
  (interactive "sDB: \nsUser: \nsPass: ")
  (insert
   (concat
    "GRANT select,insert,update,delete ON " db " TO " user "@'%.mysqlsrc' "
    "IDENTIFIED BY '" pass "'; FLUSH PRIVILEGES;")))

(defun smbconf ()
  (interactive)
  (find-file "/multi:ssh:lang@iago:sudo:root@iago:/etc/samba/smb.conf"))

(defun apachehook ()
  (interactive)
  (find-file "/hook:/service/apache-php5/php5.conf"))

(defun etchosts ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun reddit ()
  (interactive)
  (insert "0.0.0.0 reddit.com\n0.0.0.0 programming.reddit.com\n"))

(defun ykk-setup ()
  (interactive)
  (insert-file "~/proj/ykk/bin/ykk-development.s48"))

(defun bugnotes ()
  (interactive)
  (find-file "~/Documents/technology-ownership/ykk-implementation-notes.tex"))

(provide 'shortcuts-for-lang)
