(require 'lsp)

(defun lsp-describe-thing-in-window-below ()
  (interactive)
  (split-window-below)
  (lsp-describe-thing-at-point)
  ;; there is probably a way to do the following without temporarily moving to the lsp
  ;; help window, but instead specifying that window in the fit-window-to-buffer call, but
  ;; I don't know how to do that
  ;; Also: I would like to set the major mode of the lsp-help buffer to the current major
  ;; mode, but I can't figure out how to do that
  ;; TODO: make the new window a dedicated window so other buffers don't get opened there
  (save-selected-window
    (pop-to-buffer "*lsp-help*")
    (tab-line-mode -1)
    (fit-window-to-buffer nil 16)
    ))
