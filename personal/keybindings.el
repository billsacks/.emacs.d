;; General keybinding notes:
;;
;; I'm using A-* keybindings (where I have bound the option key to A) for:
;; - editing (inserting unicode characters, the sort of things that ctrl and meta are
;;   often used for, etc.)
;; - things that I might want to do multiple times in succession (because it's more
;;   convenient to do that with a modifier key than with a prefix key)
;;   - Update: but for these commands that I want to do multiple times in succession, I
;;     can also use the 'repeat' command (which I'm binding to A-g), so I can do it the
;;     first time using some other command that's hard to repeat, then repeat it with A-h.
;;
;; I'm using H-* keybindings (where I have bound the home and ESC keys to H) for commands
;; where I want a simple, one-key binding (grep, switching frames, etc.).
;;
;; I'm also defining some C-x * keybindings (particularly things that are similar to other
;; C-x keybindings) as well as C-c * keybindings (so far for enabling/disabling modes).
;;
;; I also have some super (s-*) keybindings. These are generally used to group together
;; similar commands. For example, s-w groups together some window-related commands.
;;
;; In the future I could see adding C-return or M-return as a prefix for some commands
;; (though note that these conflict with default key bindings in org mode).

(require 'macros)

;; With command acting as meta, escape is more useful as hyper rather than meta. But keep
;; ability to get escape with shift-escape if I really need it. Also bind "home" and "end"
;; to modifiers because home and end aren't very useful in emacs. Note that esc and home
;; are symmetrical, which is why I have bound them both to hyper. It would probably be
;; okay to just have one hyper, but I think there is more benefit than harm in assigning
;; both of these keys to hyper.
;;
;; Note about my choice of having hyper be the symmetrical one rather than super: With
;; super being mainly used for things where I'll hit a few keys in a row, I've found it
;; doesn't feel intuitive to have it on both sides of the keyboard: my brain thinks about
;; the two or three letters I want to type for the command, and so then it isn't automatic
;; which side to use for the super key. So I'm putting hyper in the place that is slightly
;; harder to reach but symmetrical, and super in the place that is slightly easier to
;; reach and asymmetrical.
(define-key key-translation-map (kbd "ESC") #'event-apply-hyper-modifier)
(define-key key-translation-map (kbd "S-<escape>") (kbd "ESC"))
(define-key key-translation-map (kbd "<home>") #'event-apply-hyper-modifier)
(define-key key-translation-map (kbd "<end>") #'event-apply-super-modifier)

;; These default key bindings aren't very useful, and it would be more useful to use the
;; Mac's built-in keybindings to cycle windows
;;
;; by default, M-` is tmm-menubar
(global-unset-key (kbd "M-`"))
(global-set-key (kbd "M-`") 'other-frame)
;; by default, M-~ is not-modified
(global-unset-key (kbd "M-~"))
(global-set-key (kbd "M-~") 'my-other-frame-reverse)

;; This isn't mnemonic, but it's easy to press repeatedly.
(global-set-key (kbd "A-h") 'repeat)

;; The main motivation for this key binding is to have an easier way to recenter during
;; isearch, which is something I frequently want to do. This shift-return keybinding works
;; well when I'm going through isearch with the arrow keys (which I have customized to go
;; through the isearch results). So I initially thought I'd just do this binding in the
;; isearch mode map. However, I think I'll try just doing it globally: it might be nice in
;; other contexts, too.
(global-set-key (kbd "<S-return>") 'recenter-top-bottom)
(define-key org-mode-map (kbd "<S-return>") nil)

(global-set-key (kbd "M-R") 'my-move-cursor-to-bottom)

;; handy keybindings for one-handed scrolling through code (similar to the built-in C-up
;; and C-down, but by defun)
(global-set-key (kbd "<C-S-up>") 'beginning-of-defun)
(global-set-key (kbd "<C-S-down>") 'end-of-defun)

;; use arrow keys for next & prev results in isearch
(define-key isearch-mode-map [down]
  'isearch-repeat-forward)
(define-key isearch-mode-map [up]
  'isearch-repeat-backward)
;; I often want to recenter during isearch. When using up & down to scroll through
;; results, it's awkward to hit C-l. This Shift-Return binding is easier. (S-SPC is even
;; easier. However, I'm going to try also having this available in the global key map, and
;; S-SPC is something I too often type by accident while typing a space after a capital
;; letter.) (Note: I does *not* seem sufficient to define this in the global
;; key map: it seems I need to add it to isearch-mode-map as well.)
(define-key isearch-mode-map (kbd "<S-return>")
  'recenter-top-bottom)

;; ivy/swiper key-bindings
(global-set-key (kbd "C-S-s") 'swiper)
(global-set-key (kbd "C-S-r") 'swiper-backward)
;; this is useful when you don't want to use the completion candidate, but just want to
;; use the current input, such as inputting file name patterns and directories for rgrep;
;; it is bound to C-M-j by default, but C-Return is easier
(require 'ivy)
(define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

;; Avy key-bindings
;; Note that you can use this for goto-line by entering numbers, so it's safe to override
;; the built-in M-g g for goto-line
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
;; 'f' isn't mnemonic here, but it is easy to press after M-g
(global-set-key (kbd "M-g f") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g M-f") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "A-s") 'avy-goto-word-or-subword-1)

;; Shortcuts to some Projectile things
(global-set-key (kbd "H-g") 'projectile-grep)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)
(global-set-key (kbd "C-x M-b") 'projectile-switch-to-buffer)

;; Unicode
;; en-dash (using the standard Mac key binding for this, since opt is A)
(global-set-key (kbd "A--") (kbd "C-x 8 _ n"))

;; smerge-mode
(eval-after-load 'smerge-mode
  (lambda ()
    (define-key smerge-mode-map (kbd "s-r") smerge-basic-map)
    (define-key smerge-mode-map (kbd "A-r") 'smerge-refine)))

;; Some other convenient shortcuts

;; Override standard M-y with counsel-yank-pop
(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; This is a useful command, but the C-M-SPC binding is awkward
;; M-W is nice because I often want to do M-w immediately afterwards, and this is easy (just release shift and type 'w' again)
(global-set-key (kbd "M-W") 'easy-mark)

(global-set-key (kbd "A-M-o") 'crux-smart-open-line-above)

;; some shortcuts related to ediff
(global-set-key (kbd "H-d") 'eregistry)

(global-set-key (kbd "H-G") 'vc-git-grep)
;; mnemonic for 'a' in the following: 'arguments' (but I may replace H-a with something like s-l d)
(global-set-key (kbd "H-a") 'lsp-describe-thing-in-window-below)
(global-set-key (kbd "H-v") 'delete-other-windows-vertically)

(require 'flycheck)
(global-unset-key (kbd "s-c"))
(global-set-key (kbd "s-c c") 'flycheck-buffer)
(global-set-key (kbd "s-c C") 'flycheck-clear)
(global-set-key (kbd "s-c n") 'flycheck-next-error)
(global-set-key (kbd "s-c p") 'flycheck-previous-error)
(global-set-key (kbd "s-c l") 'flycheck-list-errors)
(global-set-key (kbd "s-c h") 'my-flycheck-display-error-at-point)

;; the following will turn on hi-lock mode; to unhighlight just one, use C-x w r or M-s h u
(global-set-key (kbd "H-h") 'highlight-symbol-at-point)
(global-set-key (kbd "H-H") 'unhighlight-all-in-buffer)

;; bury buffer is a convenient way to remove a buffer from the tab line of one frame
;; without completely killing the buffer
(global-set-key (kbd "H-w") 'bury-buffer)

;; rename-uniquely is especially helpful in grep buffers
(global-set-key (kbd "H-u") 'rename-uniquely)

;; these are convenient ways to cycle through tabs in the tab line (by default, M-left and
;; M-right do the same thing as C-left and C-right, so it seems okay to rebind them; these
;; M-left and M-right bindings are consistent with iterm2, if cmd sends meta)
(global-set-key (kbd "<M-left>") 'previous-buffer)
(global-set-key (kbd "<M-right>") 'next-buffer)
;; but I need to then redefine them in org mode
(define-key org-mode-map (kbd "<M-left>") nil)
(define-key org-mode-map (kbd "<M-right>") nil)
(define-key org-mode-map (kbd "<M-S-left>") 'org-metaleft)
(define-key org-mode-map (kbd "<M-S-right>") 'org-metaright)
(define-key org-mode-map (kbd "<M-A-left>") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "<M-A-right>") 'org-shiftmetaright)
;; these bindings are shadowed by windmove mode
(define-key org-mode-map (kbd "<A-S-left>") 'org-shiftleft)
(define-key org-mode-map (kbd "<A-S-right>") 'org-shiftright)
(define-key org-mode-map (kbd "<A-S-up>") 'org-shiftup)
(define-key org-mode-map (kbd "<A-S-down>") 'org-shiftdown)

(define-key org-mode-map (kbd "C-c c") 'my-org-select-inline-code)

;; mnemonic: o for "outline" (this is similar to org-get-outline-path)
(define-key org-mode-map (kbd "H-o") 'my-org-show-position-in-text)

(global-set-key (kbd "A-f") 'forward-symbol)
(global-set-key (kbd "A-b") 'my-backward-symbol)
(global-set-key (kbd "M-A-f") 'my-forward-to-whitespace)
(global-set-key (kbd "M-A-b") 'my-backward-to-whitespace)

;; helpful when a line is wrapped
(global-set-key (kbd "A-n") 'next-logical-line)
(global-set-key (kbd "A-p") 'previous-logical-line)

(global-set-key (kbd "<M-next>") 'scroll-up-by-3)
(global-set-key (kbd "<M-prior>") 'scroll-down-by-3)
(global-set-key (kbd "<A-M-next>") 'scroll-up-by-10)
(global-set-key (kbd "<A-M-prior>") 'scroll-down-by-10)

(global-set-key (kbd "A-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "A-t") 'indent-relative)
(global-set-key (kbd "A-SPC") 'cycle-spacing)

;; Some C-c key bindings. So far I'm using these to enable / disable certain modes.
;; (2021-05-18) I have noticed that C-c bindings are sometimes used for mode-specific
;; bindings (e.g., for org-mode and python-mode). So I'm thinking about keeping with that
;; idea, and changing these bindings that enable / disable modes to something like s-x.
;;
;; This is useful to disable auto-triggering of company in buffers where that causes a big slowdown (e.g., case.py)
(global-set-key (kbd "C-c c") 'my-toggle-company-idle-delay)
(global-set-key (kbd "C-c f") 'auto-fill-mode)
(global-set-key (kbd "C-c i") 'highlight-indent-guides-mode)
(global-set-key (kbd "C-c I") 'my-toggle-highlight-indent-guides-responsive)
(global-set-key (kbd "C-c s") 'scroll-lock-mode)
(global-set-key (kbd "C-c v") 'view-mode)
;; This is helpful with files that magit opens from a different revision:
;; which-function-mode doesn't get enabled properly in these files.
(global-set-key (kbd "C-c w") 'my-enable-which-function-mode)

(global-set-key (kbd "H-i") 'counsel-imenu)
(global-set-key (kbd "H-I") 'lsp-ui-imenu)
(global-set-key (kbd "H-M-i") 'imenu-anywhere)
(global-set-key (kbd "H-f") 'select-frame-by-name)
(global-set-key (kbd "H-F") 'new-frame-with-scratch-buffer)
;; The following should probably just be enabled in ediff mode; its purpose is for
;; avoiding the highlighting of an ediff session contaminating my view elsewhere -
;; particularly if I have the same buffer open in two ediff sessions (in two different
;; frames).
(global-set-key (kbd "H-M-f") 'my-ediff-unhighlight-and-switch-frames)

(global-set-key (kbd "H-m") 'move-buffer-to-other-window)
(global-set-key (kbd "H-M-m") 'copy-buffer-to-other-window)
(global-set-key (kbd "H-M") 'crux-swap-windows)

;; s-w is originally set to the same thing as C-x o: ace-window
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "s-w x") 'maximize-window)
(global-set-key (kbd "s-w n") 'minimize-window)
;; mnemonic for the following: "equal"
(global-set-key (kbd "s-w e") 'balance-windows)
;; mnemonic for the following: wider
(global-set-key (kbd "s-w w") 'enlarge-window-50)
(global-set-key (kbd "s-w <left>") 'winner-undo)
(global-set-key (kbd "s-w <right>") 'winner-redo)

;; Some workarounds for issues (at least with emacsformacosx)

;; Sometimes the screen goes mostly blank and needs redrawing
(global-set-key (kbd "H-r") 'redraw-display)

;; Sometimes scroll bars disappear when resizing a frame; this function fixes them; note
;; that we use the same modifiers as for Divvy, since this happens after using Divvy
(global-set-key (kbd "A-C-M-s") 'fix-scroll-bars)
