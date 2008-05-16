(defun mysqlgrant (db user pass)
  (interactive "sDB: \nsUser: \nsPass: ")
  (insert
   (concat
    "GRANT select,insert,update,delete ON " db ".* TO " user "@'%.mysqlsrc' "
    "IDENTIFIED BY '" pass "'; FLUSH PRIVILEGES;")))

(defun jerk-smb-conf ()
  (interactive)
  (find-file "/multi:ssh:lang@iago:sudo:root@jerk:/etc/samba/smb.conf"))

(defun cerf-apache-php5 ()
  (interactive)
  (find-file "/cerf:/service/apache-php5/php5.conf"))

(defun etc-hosts ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun reddit ()
  (interactive)
  (insert
   "0.0.0.0 slashdot.org\n"
   "0.0.0.0 reddit.com\n"
   "0.0.0.0 programming.reddit.com\n"))

(defun ykk-setup ()
  (interactive)
  (insert-file "~/proj/ykk/bin/ykk-development.s48"))

(defun bugnotes ()
  (interactive)
  (find-file "~/Documents/technology-ownership/ykk-implementation-notes.tex"))

(defun apache-htaccess-login ()
  (interactive)
  (insert "Order allow,deny
Allow from 74.93.41.41
Deny from all
AuthType Basic
AuthName Login Required
AuthUserFile /usr/local/apache/etc/.htpasswd-allusers
Require valid-user
Satisfy any"))

(provide 'shortcuts-for-lang)
