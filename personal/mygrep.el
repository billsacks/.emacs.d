(require 'grep)
(require 'compile)

;; I can't seem to access the command argument to compilation-start from within
;; grep-changes, even though it's called from within compilation-start. I'm dealing with
;; that by adding advice around compilation-start to make that argument available. It
;; really feels like this shouldn't be necessary.
(defun wrap-compilation-start (orig-fun command &rest args)
  (let ((res (apply orig-fun command args)))
    res))
(advice-add 'compilation-start :around 'wrap-compilation-start)

;; Do an occur search for header lines in the git grep output (when running git grep
;; --show-function); this gives something roughly equivalent to git grep
;; --files-with-matches, though with a separate entry for each function rather than just
;; one per file
(defun git-grep-header-occur ()
  (interactive)
  (occur "=[0-9]+="))

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
  (defface my-tiny-grep-hit-face
    '((t
       :inherit compilation-info-face
       ;; Super tiny: I do a git grep with --show-function, which gives the file name and
       ;; function in a header. So the file name on each line is unnecessary and just makes
       ;; the lines long. I wanted to get rid of it entirely, e.g., by making it invisible,
       ;; but I can't figure out how to do that. So do this kludge of setting it to tiny.
       :height 6
       ))
    "My face for grep hit"
    :group 'my-grep-faces)
  (set (make-local-variable 'grep-context-face) 'my-grep-context-face)
  (set (make-local-variable 'grep-match-face) 'my-grep-match-face)
  (if (string-match-p (regexp-quote "--show-function") command)
      (set (make-local-variable 'compilation-error-face) 'my-tiny-grep-hit-face))
  ;; In the following, 'f' stands for 'files' (as in 'files-with-matches')
  (local-set-key (kbd "f") #'git-grep-header-occur)
  )
(add-hook 'grep-setup-hook 'grep-changes)
