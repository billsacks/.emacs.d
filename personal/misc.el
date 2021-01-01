;; needed to use emacsclient - which in turn is needed to use aquamacs as the git editor
;; (from http://superuser.com/questions/180799/using-aquamacs-for-editor-in-subversion/180818#180818)
(server-start)

;; recommended for LSP mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; For flycheck to display in the margins
(setq-default left-margin-width 1)

;; For diff-hl
;; This is needed according to https://github.com/dgutov/diff-hl; note
;; that the post-commit hook is already defined in prelude
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;; This allows diff-hl to show diffs even in unsaved buffers
(diff-hl-flydiff-mode)
