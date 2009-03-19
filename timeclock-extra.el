(defun timeclock-projects-to-seconds ()
  (mapcar (lambda (proj)
            (cons (car proj)
                  (apply '+ (mapcar 'timeclock-entry-length
                                    (cdr proj)))))
          (timeclock-project-alist)))

(defun round-up (arg &optional divisor)
  (let* ((divisor (or divisor 10))
         (round (round arg divisor))
         (multi (* round divisor)))
    (if (> arg multi)
        (+ 1 round)
      round)))

(assert (equal (round-up 46 15) 4))

(defun timeclock-project-summary-map (display-pair)
  (mapcar (lambda (pair)
            (funcall display-pair
                     (car pair)
                     (/ (* (round-up (/ (cdr pair) 60) 15)
                           15)
                        60.0)))
          (timeclock-projects-to-seconds)))

(defun timeclock-project-summary ()
  "Insert a summary of of all projects containing only total
time, rounded to the quarter hour."
  (interactive)
  (let ((total 0.0))
    (timeclock-project-summary-map
     (lambda (proj time)
       (progn
         (insert (format "%s, %.2f\n" proj time))
         (setq total (+ total time)))))
    (insert (format "%s, %.2f\n" "total" total))))

(provide 'timeclock-extra)
