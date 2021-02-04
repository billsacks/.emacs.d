(require 'lsp)

;; I could imagine creating a similar function to this that does a go-to-definition in a
;; little window created below. One big value of that would be to facilitate seeing the
;; definition of a type-bound subroutine in Fortran, for which lsp-describe-thing-at-point
;; currently doesn't work (although, to get this to work, I would need to chain
;; go-to-definition and go-to-implementation, I think).
(defun lsp-describe-thing-in-window-below ()
  (interactive)
  (split-window-below)
  (lsp-describe-thing-at-point)
  ;; there is probably a way to do the following without temporarily moving to the lsp
  ;; help window, but instead specifying that window in the fit-window-to-buffer call, but
  ;; I don't know how to do that
  ;; Also: I would like to set the major mode of the lsp-help buffer to the current major
  ;; mode, but I can't figure out how to do that
  (save-selected-window
    (pop-to-buffer "*lsp-help*")
    (tab-line-mode -1)
    (fit-window-to-buffer nil 16)
    ))
