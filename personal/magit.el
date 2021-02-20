(require 'magit)

;; Do something like magit-show-refs ('y' from the magit status buffer), but only showing
;; local branches, not remote branches or tags (this is achieved by removing
;; magit-insert-remote-branches and magit-insert-tags from magit-refs-sections-hook). The
;; point of this is that I want margin information by default for my branches, but I often
;; want to avoid the slowness of adding margin information to all of the remote branches
;; (which can take around 10 seconds in some of my repositories).
(defun my-magit-show-local-refs ()
  (interactive)
  (let ((magit-refs-sections-hook '(magit-insert-error-header magit-insert-branch-description magit-insert-local-branches)))
    ;; the following line is copied from magit-refs.el, in the definition of
    ;; magit-show-refs: this is what happens by default when you run magit-show-refs ('y'
    ;; from the magit status buffer)
    (magit-refs-setup-buffer "HEAD" (magit-show-refs-arguments))))
