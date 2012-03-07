(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))

(defvar mode-line-show-minor-modes nil)
(defvar powerline-default-mode-line mode-line-format)

(let ((color0 "#333") (color1 "#666") (color2 "#999"))
  (setq
   arrow-right-1
   (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center)
   arrow-right-2
   (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center)
   arrow-left-1
   (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center)
   arrow-left-2
   (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

  (setq-default
   mode-line-format
   (list
    '(:eval (concat (propertize " %* %I %b" 'face 'mode-line-color-1)
                    (propertize " " 'display arrow-right-1)))
    '(:eval (concat (propertize
                     " %[%m%] "
                     'face 'mode-line-color-2
                     'mouse-face 'mode-line-color-2
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq mode-line-show-minor-modes
                                                  (not mode-line-show-minor-modes))
                                            (redraw-modeline))))
                    (propertize " " 'display arrow-right-2)))
    '(:eval (if mode-line-show-minor-modes
                mode-line-modes
              global-mode-string))

    ;; Justify right by filling with spaces to right fringe - 16
    ;; (16 should be computed rahter than hardcoded)
    '(:eval (propertize " " 'display '((space :align-to (- right-fringe 20)))))

    '(:eval (concat (propertize " " 'display arrow-left-2)
                    (propertize " %6p " 'face 'mode-line-color-2)))
    '(:eval (concat (propertize " " 'display arrow-left-1)
                    (propertize "%4l:%2c      " 'face 'mode-line-color-1)))
    ))

  (make-face 'mode-line-color-1)
  (set-face-attribute 'mode-line-color-1 nil
                      :foreground "#fff"
                      :background color1)

  (make-face 'mode-line-color-2)
  (set-face-attribute 'mode-line-color-2 nil
                      :foreground "#fff"
                      :background color2)

  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background color0
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#ccc"
                      :background color2))

(provide 'powerline)
