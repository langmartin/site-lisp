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

(defun rebuild-site-lisp-for-desktop ()
  (interactive)
  (shell-command "cd /usr/share/emacs/site-lisp; ./.all.sh /Applications/Emacs.app/Contents/MacOS/Emacs"))

(defun coptix-local-python ()
  (interactive)
  (find-file "/coptix/local/lib/python2.5/distutils/distutils.cfg"))

(defun coptix-local-python-path ()
  (interactive)
  (find-file "/coptix/local/lib/python2.5/site-packages/sitecustomize.py"))

(defgroup cx-scripts nil
  "Script configuration"
  :group 'tools)

(defcustom sshfs-local-path "~/code/mount"
  "Local path for sshfs mounts"
  :group 'cx-scripts
  :type 'file)

(defcustom sshfs-executable "/Applications/sshfs/bin/mount_sshfs"
  ""
  :group 'cx-scripts
  :type 'file)

(defun sshfs-techname (host)
  (mapcar (lambda (c)
           (case c
             ((?/ ?:) ?-)
             (t c)))
          host))

(defun sshfs (host)
  (interactive "sHost/Path: ")
  (let ((path (concat sshfs-local-path "/" (sshfs-techname host))))
    (make-directory path t)
    (shell-command (concat sshfs-executable " " host " " path))))

(provide 'shortcuts-for-lang)

