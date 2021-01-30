;; needed to use emacsclient - which in turn is needed to use aquamacs as the git editor
(server-start)

;; recommended for LSP mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

;; I came across these settings while looking up information on how to improve performance
;; with long lines in emacs, wondering if long lines in visual-line-mode was contributing
;; to typing lags when editing markdown files. It sounds like these are unlikely to make
;; much difference with the only moderately-long lines in my files, but it also sounds
;; like this won't hurt. (See
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
;; and
;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
