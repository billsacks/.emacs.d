;; needed to use emacsclient - which in turn is needed to use aquamacs as the git editor
(server-start)

;; recommended for LSP mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; For flycheck to display in the margins
(setq-default left-margin-width 1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; this is copied from prelude-ui.el, but without the "Emacs Prelude" piece
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; For diff-hl
;; This is needed according to https://github.com/dgutov/diff-hl; note
;; that the post-commit hook is already defined in prelude
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;; This allows diff-hl to show diffs even in unsaved buffers
(diff-hl-flydiff-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'prog-mode-hook '(lambda() (set-fill-column 90)))
(add-hook 'org-mode-hook '(lambda()
                            (auto-fill-mode -1)
                            (visual-line-mode +1)
                            (set-fill-column 110)))
(add-hook 'markdown-mode-hook '(lambda()
                                 (auto-fill-mode -1)
                                 (visual-line-mode +1)
                                 (set-fill-column 110)))

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
;; I think the following would normally be done by default, but crux's C-a shadows it (C-e
;; works fine already, giving end-of-visual-line)
(add-hook 'visual-line-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-a"))
            (local-set-key (kbd "C-a") 'beginning-of-visual-line)))

;; Workaround: wrapped lines in lsp-ui-imenu break the ability to move from line to line
;; with the arrow keys; so while that continues to be a problem, prevent wrapping
(add-hook 'lsp-ui-imenu-mode-hook
          (lambda ()
            (setq-local truncate-lines 1)))

(load-library "cime_tools")

(require 'org-protocol)
(require 'rg)
(rg-enable-default-bindings)

;; This is useful when I have grep results (or similar) displayed in a window that is
;; taking up much of the screen: it prevents the grep results from being split left-right.
;; In general, I don't want emacs splitting my window for me: I want to be in control of
;; that.
(setq split-width-threshold nil)
