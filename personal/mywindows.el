(defun my-frame-non-text-width ()
  "Get the width of the non-text portion of a frame"
  (- (frame-outer-width) (frame-text-width)))

(defun my-frame-full-width ()
  "Get the text area width of a frame that takes up the entire screen"
  (- (display-pixel-width) (my-frame-non-text-width)))

(defun my-frame-half-width ()
  "Get the text area width of a frame that takes up half the screen"
  (- (/ (display-pixel-width) 2) (my-frame-non-text-width)))

(defun my-make-frame-full-and-split ()
  "Make the current frame full-width and split left-right"
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-width (selected-frame) (my-frame-full-width) nil t)
  (split-window-right))

(defun my-make-frame-half-left ()
  "Make the current frame half-width on the left, with a single window"
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (delete-other-windows)
  (set-frame-width (selected-frame) (my-frame-half-width) nil t))

(defun my-make-frame-half-right ()
  "Make the current frame half-width on the right, with a single window"
  (interactive)
  (set-frame-position (selected-frame) (/ (display-pixel-width) 2) 0)
  (delete-other-windows)
  (set-frame-width (selected-frame) (my-frame-half-width) nil t))
