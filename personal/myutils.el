;; general-purpose utility functions

(defun make-or-switch-to-frame (frame-name)
  "Switch to frame with the given name if it exists; otherwise, create a frame with that name

  One down side of setting the name is that it prevents the display of the full file path,
  as is typically achieved via frame-title-format. I can't see any way around that issue."
  (let ((cur-buffer (current-buffer)))
    (if (condition-case nil
            ;; try to call select-frame-by-name with the given name; if it raises an error,
            ;; ignore the error and return t
            (select-frame-by-name frame-name)
          (error t))
        ;; if an error was returned, then make a new frame and give it the specified name
        (progn (select-frame (make-frame))
               ;; it should be possible to set the name in the make-frame call, but I can't get that to work
               (set-frame-parameter nil 'name frame-name)
               (split-window-right))

      ;; if no error was returned, then we have selected that frame; in that case, display
      ;; the current buffer in that frame (this is important for magit, so that a
      ;; following magit-status command will operate on the correct project)
      (set-buffer cur-buffer)

      )
    )
  )
