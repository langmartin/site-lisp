(defun mysqlgrant (db user pass)
  (interactive "sDB: \nsUser: \nsPass: ")
  (insert
   (concat
    "GRANT select,insert,update,delete ON " db ".* TO " user "@'%.mysqlsrc' "
    "IDENTIFIED BY '" pass "'; FLUSH PRIVILEGES;")))

(defun iago-smb-conf ()
  (interactive)
  (find-file "/multi:ssh:lang@iago:sudo:root@iago:/etc/samba/smb.conf"))

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
   "0.0.0.0 reddit.com\n"))

(defun ykk-setup ()
  (interactive)
  (insert-file "~/proj/ykk/bin/ykk-development.s48"))

(defun ykk-notes ()
  (interactive)
  (find-file "~/Documents/technology-ownership/ykk-implementation-notes.tex"))

(defun htaccess-login ()
  (interactive)
  (insert "Order Allow,Deny
Allow from 75.148.111.133
Deny from all
AuthType Basic
AuthName \"Login Required\"
AuthUserFile /usr/local/apache/etc/.htpasswd-allusers
Require valid-user
Satisfy any"))

(defun htaccess-extension-chop ()
  (interactive)
  (insert "Options -MultiViews
RewriteEngine on
RewriteBase /
RewriteRule ^catalog/catalog/(.*)$ /images/catalog/catalog-non-ftp/$1 [L]
RewriteCond %{REQUEST_FILENAME} !-f
RewriteCond %{REQUEST_FILENAME} !-d
RewriteCond %{REQUEST_URI} ^[^.]*$
RewriteRule ^(.*)$ $1.php [QSA,L]"))

(provide 'shortcuts-for-lang)
