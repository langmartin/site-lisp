;; -*- no-byte-compile: t -*-

(defun mysqlgrant (db user pass)
  (interactive "sDB: \nsUser: \nsPass: ")
  (insert
   (concat
    "GRANT select,insert,update,delete ON " db ".* TO " user "@'%.mysqlsrc' "
    "IDENTIFIED BY '" pass "'; FLUSH PRIVILEGES;")))

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
  (insert "Order Deny,Allow
Allow from 75.148.111.133
Deny from all
AuthType Basic
AuthName \"Webtools\"
AuthUserFile /coptix/admin/scripts/share/htpasswd
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

(fset 'tex-define-to-rst
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 92 105 116 101 109 13 M-backspace backspace 4 134217828 67108911 134217820 19 93 13 backspace return return 21 32 134217841 16 11] 0 "%d")) arg)))

(defun share-itunes ()
  (interactive)
  (shell-command "chmod -R g+rwX,o+rX ~friends/Music")
  (shell-command "chmod -R g+rwX,o+rX ~friends/Pictures"))

(defun flush-cache ()
  (interactive)
  (shell-command "dscacheutil -flushcache"))

(defun hibernate ()
  (interactive)
  (shell-command "/Users/lang/bin/hiber"))

(defun shell-concat (lst)
  (apply 'concat
         (intersperse
          (mapcar (lambda (el)
                    (concat "\"" el "\""))
                  lst)
          " ")))

(progn
  (setq backup-create-tar
        '(".bbdb"
          ".chrome"
          ".emacs"
          ".emacs.bmk"
          ".emacs.d"
          ".eshell"
          ".git"
          ".gitconfig"
          ".history"
          ".plan"
          ".session"
          ".ssh"
          "bin"
          "code"
          "contrib"
          "documents"
          "images"
          "/f/menu"
          ))
  (setq backup-encrypted-tar "e:/backup")
  (defun backup-encrypted-tar ()
    (interactive)
    (save-default-directory
        "~"
      (shell-command
       (concat
        "tar cz " (shell-concat backup-create-tar)
        "|"
        "gpg -e -r \"Lang Martin\" -o " (make-temp-name backup-encrypted-tar)
        "&")))))

(provide 'lang-scripts)
