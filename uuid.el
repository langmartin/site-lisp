;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;

;;
;; uuid.el - This tiny emacs extension defines a new command,
;; insert-random-uuid, which will insert a randomly generated
;; UUID at the point.
;;

(defun generate-random-hex-string (length)
  (let (result (digits "0123456789abcdef"))
    (dotimes (number length result)
      (setq result (cons (elt digits (random 16)) result)))
    (concat result)))

(defun generate-random-uuid ()
  "Generate a random UUID."
  (mapconcat 'generate-random-hex-string (list 8 4 4 4 12) "-"))

(defun insert-random-uuid ()
  "Insert a random UUID at the point."
  (interactive)
  (insert (generate-random-uuid)))

(provide 'uuid)
