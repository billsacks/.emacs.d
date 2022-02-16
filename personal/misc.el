;; needed to use emacsclient - which in turn is needed to use aquamacs as the git editor
(require 'server)
(unless (server-running-p) (server-start))

;; For editing Chrome text boxes in emacs
(edit-server-start)

(require 'smart-mode-line)
(sml/setup)

;; recommended for LSP mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq-default left-margin-width 1)

;; make scratch buffer be org-mode
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; this is copied from prelude-ui.el, but without the "Emacs Prelude" piece
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add imenu to menu bar whenever font-lock-mode is enabled
;; Note that font lock mode is enabled for any major mode,
;; so this means try to add imenu for every major mode
;; (from http://www.emacswiki.org/cgi-bin/wiki/ImenuMode)
;;
;; Note (2021-11-16) This seems to cause typing lag, at least if imenu-auto-rescan is t;
;; for now I'm setting imenu-auto-rescan to nil (the default) to avoid this lag, because I
;; would like to have this in the menu bar.
(defun my-add-imenu ()
  (interactive)
  (condition-case nil (imenu-add-to-menubar "imenu") (error nil)))
;; (add-hook 'font-lock-mode-hook 'my-add-imenu)
;;
;; Note (2022-02-16) which-func and imenu cause magit to hang in some situations (the
;; entire emacs instance hangs, and the fan spins, suggesting that there is an infinite
;; loop trying to do something). I specifically notice this when trying to view a
;; particular commit (via magit-show-commit, or hitting return on a line when viewing a
;; git blame) - for example commit 90741806a in CTSM. To work around this until this is
;; fixed in magit, I am (1) commenting out the above add-hook, (2) making my-add-imenu
;; interactive so I can still get it when I want it, and (3) explicitly listing modes for
;; which I want which-func enabled, rather than letting it be enabled in all modes. This
;; is a pain, but having emacs hang and needing to force quit it is a bigger pain.
;; (Ideally there would be a way to say: enable which-func for all modes EXCEPT magit, but
;; I don't see a way to do that.) (Also, ideally, I could call my-add-imenu whenever I'm
;; in a mode where which-function is enabled, or something like that; but since I don't
;; care that much about having imenu in the menubar, I haven't tried to figure out how to
;; do that.) However, maybe I'll decide that it isn't such a bad thing to explicitly list
;; the modes in which which-function-mode is enabled: maybe it will speed things up in
;; other modes.
(require 'which-func)
(setq which-func-modes '(c-mode c++-mode emacs-lisp-mode f90-mode org-mode perl-mode python-mode))

;; normally this would take three applications of C-l
(defun my-recenter-to-bottom()
  (interactive)
  (recenter-top-bottom -1))

;; normally this would take three applications of M-r
(defun my-move-cursor-to-bottom()
  (interactive)
  (move-to-window-line -1))

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
  (interactive)
  (variable-pitch-mode +1)
  (auto-fill-mode -1)
  (visual-line-mode +1)
  (adaptive-wrap-prefix-mode +1)
  (set-fill-column 110))
(add-hook 'markdown-mode-hook 'my-document-mode-changes)
(add-hook 'rst-mode-hook 'my-document-mode-changes)
;; like my-document-mode-changes, but with some tweaks for org mode
(defun my-org-mode-changes()
  (interactive)
  ;; no variable-pitch-mode: we'll use org-variable-pitch-minor-mode instead
  (auto-fill-mode -1)
  (visual-line-mode +1)
  (set-fill-column 110)

  ;; show-paren-mode has issues with org-indent-mode: lines sometimes jump to the left
  ;; margin when parentheses are being matched. show-paren-mode is apparently a global
  ;; mode; it isn't enabled initially in org-mode, but if a programming-mode buffer has
  ;; already been opened that enables it, then it gets enabled globally, including in the
  ;; org mode buffers. So disable show-paren-mode in org mode to work around this issue.
  (make-local-variable 'show-paren-mode)
  (setq show-paren-mode nil)
  )
(add-hook 'org-mode-hook 'my-org-mode-changes)
;; See https://github.com/cadadr/elisp/blob/stable/org-variable-pitch.el
(require 'org-variable-pitch)
(add-hook 'after-init-hook #'org-variable-pitch-setup)
(add-hook 'org-mode-hook 'org-appear-mode)

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

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

;; Suppress undo tree's messages when you save a buffer. These messages were annoying
;; because the file name was long, resulting in the minibuffer momentarily expanding to
;; two lines, which was distracting. From
;; https://emacs.stackexchange.com/questions/59942/is-it-possible-suppress-save-message-for-undo-tree.
(defun my-undo-tree-save-history (undo-tree-save-history &rest args)
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply undo-tree-save-history args)))
(advice-add 'undo-tree-save-history :around 'my-undo-tree-save-history)

(load-library "cime_tools")

(require 'org-protocol)
(require 'org-inlinetask)
(require 'rg)
(rg-enable-default-bindings)

(require 'highlight-indent-guides)
;; It works to toggle this on while highlight-indent-guides is active, but to toggle it
;; off, you need to restart highlight-indent-guides.
(defun my-toggle-highlight-indent-guides-responsive ()
  (interactive)
  (if highlight-indent-guides-responsive
      (setq highlight-indent-guides-responsive nil)
    (setq highlight-indent-guides-responsive 'top)))

(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

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
  (let ((vec (vconcat "c")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat "C")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat "g")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat "m")))
    (aset vec 0 (logior (aref vec 0) ?\A-\^@))
    (define-key key-translation-map vec nil))
  (let ((vec (vconcat "o")))
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
