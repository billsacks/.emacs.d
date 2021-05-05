(require 'grep)
(require 'compile)

;; Do an occur search for header lines in the git grep output (when running git grep
;; --show-function); this gives something roughly equivalent to git grep
;; --files-with-matches, though with a separate entry for each function rather than just
;; one per file
(defun git-grep-header-occur ()
  (interactive)
  (occur "=[0-9]+="))

;; To save screen width, we make the file names tiny by default. This often makes sense
;; when grep is run with --show-function - so the file name is already available in the
;; header of each section. However, sometimes the context isn't available, so the
;; following function can be used to toggle small vs. regular sized file names.
(defun hide-file-name-toggle ()
  (interactive)
  (if (eq 120 (face-attribute 'my-grep-hit-face :height (selected-frame)))
      (set-face-attribute 'my-grep-hit-face (selected-frame) :height 6)
    (set-face-attribute 'my-grep-hit-face (selected-frame) :height 120))
  )

(defun grep-changes()
  (defface my-grep-context-face
    '((t
       :inherit font-lock-function-name-face
       ))
    "My face for grep context"
    :group 'my-grep-faces)
  (defface my-grep-match-face
    '((t
       :inherit font-lock-comment-face
       ))
    "My face for grep match"
    :group 'my-grep-faces)
  (defface my-grep-hit-face
    '((t
       ;; Super tiny: I do a git grep with --show-function, which gives the file name and
       ;; function in a header. So the file name on each line is unnecessary and just makes
       ;; the lines long. I wanted to get rid of it entirely, e.g., by making it invisible,
       ;; but I can't figure out how to do that. So do this kludge of setting it to tiny.
       :height 6
       :foreground "ForestGreen"
       ))
    "My face for grep hit"
    :group 'my-grep-faces)
  (set (make-local-variable 'grep-context-face) 'my-grep-context-face)
  (set (make-local-variable 'grep-match-face) 'my-grep-match-face)
  (set (make-local-variable 'compilation-error-face) 'my-grep-hit-face)
  ;; In the following, 'f' stands for 'file name'
  (local-set-key (kbd "f") #'hide-file-name-toggle)
  ;; In the following, 'o' stands for 'occur'
  (local-set-key (kbd "o") #'git-grep-header-occur)
  )
(add-hook 'grep-setup-hook 'grep-changes)
