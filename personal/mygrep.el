(require 'grep)

(defun grep-changes()
  (defface my-grep-context-face
    '((t
       :inherit font-lock-builtin-face
       :weight bold
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
       :inherit compilation-info-face
       ;; Super tiny: I do a git grep with --show-function, which gives the file name and
       ;; function in a header. So the file name on each line is unnecessary and just makes
       ;; the lines long. I wanted to get rid of it entirely, e.g., by making it invisible,
       ;; but I can't figure out how to do that. So do this kludge of setting it to tiny.
       :height 5
       ))
    "My face for grep hit"
    :group 'my-grep-faces)
  (set (make-local-variable 'grep-context-face) 'my-grep-context-face)
  (set (make-local-variable 'grep-match-face) 'my-grep-match-face)
  (set (make-local-variable 'compilation-error-face) 'my-grep-hit-face)
  )

(add-hook 'grep-setup-hook 'grep-changes)
