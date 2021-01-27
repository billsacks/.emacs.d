;; needed to use emacsclient - which in turn is needed to use aquamacs as the git editor
(server-start)

;; recommended for LSP mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; For flycheck to display in the margins
;; Just using a setq-default on left-margin-width isn't always enough: sometimes, when
;; splitting a window, the margins disappear. The following ensures that the margins get
;; set appropriately all the time (from
;; https://stackoverflow.com/questions/7251784/how-do-i-adjust-the-left-margin-in-emacs-nox)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 0)))

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

;; I don't like how flyspell highlights words based on where your mouse happens to be
;; (because usually my mouse cursor is not positioned intentionally); this fixes it
;; from https://emacs.stackexchange.com/questions/36899/disable-clickable-links-for-misspelled-words-flyspell
(with-eval-after-load "flyspell"
  (defun make-flyspell-overlay-return-mouse-stuff (overlay)
    (overlay-put overlay 'help-echo nil)
    (overlay-put overlay 'keymap nil)
    (overlay-put overlay 'mouse-face nil))
  (advice-add 'make-flyspell-overlay :filter-return #'make-flyspell-overlay-return-mouse-stuff)
  )

(load-library "cime_tools")

(require 'org-protocol)
(require 'rg)
(rg-enable-default-bindings)
