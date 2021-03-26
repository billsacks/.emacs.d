;; needed to use emacsclient - which in turn is needed to use aquamacs as the git editor
(require 'server)
(unless (server-running-p) (server-start))

;; recommended for LSP mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; make scratch buffer be markdown-mode
(setq initial-major-mode 'gfm-mode)
(setq initial-scratch-message nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; this is copied from prelude-ui.el, but without the "Emacs Prelude" piece
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; For diff-hl
;; This is needed according to https://github.com/dgutov/diff-hl; note
;; that the post-commit hook is already defined in prelude
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;; This allows diff-hl to show diffs even in unsaved buffers
(diff-hl-flydiff-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; clean whitespace (https://github.com/lewang/ws-butler)
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(add-hook 'prog-mode-hook '(lambda() (set-fill-column 90)))
(add-hook 'deft-mode-hook '(lambda() (set-fill-column 130)))

(defun my-document-mode-changes()
  (variable-pitch-mode)
  (auto-fill-mode -1)
  (visual-line-mode +1)
  (set-fill-column 110))
(add-hook 'markdown-mode-hook 'my-document-mode-changes)
(add-hook 'org-mode-hook 'my-document-mode-changes)
(add-hook 'rst-mode-hook 'my-document-mode-changes)

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

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
(require 'org-inlinetask)
(require 'rg)
(rg-enable-default-bindings)

;; This is useful when I have grep results (or similar) displayed in a window that is
;; taking up much of the screen: it prevents the grep results from being split left-right.
;; This is also useful to avoid having the ediff control window be split horizontally, and
;; in getting my lsp-describe-thing-in-window-below function to work well when I have a
;; single window on the frame. I could probably figure out other solutions to those
;; specific problems, but in general, I don't want emacs splitting my window left-right
;; for me: I want to be in control of that.
(setq split-width-threshold nil)

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

;; Fix for A-m sometimes making a Greek mu rather than A-m (from
;; https://emacs.stackexchange.com/questions/17508/how-can-i-prevent-override-key-translation-behavior-such-as-%C2%B5-translated-from),
;; and similarly for A-u, A-- and others
(with-eval-after-load 'iso-transl
  (let ((vec (vconcat "m")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat "u")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat "-")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat " ")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil)))
