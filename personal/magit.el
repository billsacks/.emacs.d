(require 'magit)

;; Use a dedicated frame ("magit-frame") for magit operations: Even though magit makes an
;; attempt to clean up after itself, I still find that it messes with my window setup, and
;; particularly my tab-line ordering. In addition, I often invoke ediff from magit, and
;; that really messes with my window setup - again, particularly my tab-line ordering, but
;; possibly other stuff, too. Note that I'm using a single frame for all magit operations,
;; and the idea for now is that I'll have ediff and magit stuff all appear in that frame.
(defun my-magit-status-magit-frame ()
  (interactive)
  (make-or-switch-to-frame "magit-frame")
  (magit-status-setup-buffer))

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

;; The following is useful for doing a git diff between the working tree and the
;; merge-base of HEAD and some branch
(defun my-magit-get-merge-base ()
  "Copy the merge-base of HEAD and the given branch to the kill-ring"
  (interactive)
  (kill-new (magit-git-string "merge-base" "HEAD"
                              ;; in the following, note that we default to the name of the main branch
                              (magit-read-branch-or-commit "Get merge base of HEAD and commit" (magit-main-branch)))))
