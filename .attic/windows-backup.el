(require 'srfi-1)

(defvar windows-backup-file
  "C:/Documents and Settings/lmartin/My Documents/windows-backup.zip")

(defvar windows-backup-list
  "C:/Documents and Settings/lmartin/My Documents/windows-backup.txt")

(defun windows-backup ()
  "Create a zip archive of files that should be backed up on my keychain"
  (interactive)
  (cd "~")
  (shell-command
   (apply
    'concat
    (intersperse
     (mapcar (lambda (x)
               (concat "\"" x "\""))
             (list "c:/Program Files/WinZip/WZZIP.EXE"
                   "-rp"
                   "windows-backup.zip"
                   (concat "@" "windows-backup.txt")))
     " "))))

(provide 'windows-backup)
