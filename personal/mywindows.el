(defun my-this-display-pixel-width ()
  "Get the width in pixels of the current monitor

This is like display-pixel-width, but when using dual displays just includes the current monitor

Credit: https://emacs.stackexchange.com/questions/60707/how-to-get-the-display-dimensions-of-the-display-emacs-is-in"
  (nth 3 (assq 'geometry (frame-monitor-attributes))))

(defun my-this-display-pixel-height ()
  "Get the height in pixels of the current monitor

See my-this-display-pixel-width for notes"
  (nth 4 (assq 'geometry (frame-monitor-attributes))))

(defun my-frame-non-text-width ()
  "Get the width of the non-text portion of a frame"
  (- (frame-outer-width) (frame-text-width)))

(defun my-frame-full-width ()
  "Get the text area width of a frame that takes up the entire screen"
  (- (my-this-display-pixel-width) (my-frame-non-text-width)))

(defun my-frame-half-width ()
  "Get the text area width of a frame that takes up half the screen"
  (- (/ (my-this-display-pixel-width) 2) (my-frame-non-text-width)))

(defun my-make-frame-full-and-split ()
  "Make the current frame full-width and split left-right"
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-width (selected-frame) (my-frame-full-width) nil t)
  (split-window-right))

(defun my-make-frame-full-and-transpose ()
  "Make the current frame full-width then transpose-frame"
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-width (selected-frame) (my-frame-full-width) nil t)
  (transpose-frame))

(defun my-make-frame-half-left ()
  "Make the current frame half-width on the left, with a single window"
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (delete-other-windows)
  (set-frame-width (selected-frame) (my-frame-half-width) nil t))

(defun my-make-frame-half-right ()
  "Make the current frame half-width on the right, with a single window"
  (interactive)
  (set-frame-position (selected-frame) (/ (my-this-display-pixel-width) 2) 0)
  (delete-other-windows)
  (set-frame-width (selected-frame) (my-frame-half-width) nil t))

(defun my-set-window-dedicated ()
  "Make the current window dedicated to its current buffer"
  (interactive)
  ;; Use a flag of 'yes instead of t, because t makes it strongly dedicated, which is more
  ;; than we want
  (set-window-dedicated-p (selected-window) 'yes))

(defun my-set-window-undedicated ()
  "Make the current window no longer dedicated to its current buffer"
  (interactive)
  (set-window-dedicated-p (selected-window) nil))
