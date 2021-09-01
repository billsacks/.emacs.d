;; General keybinding notes:
;;
;; I'm using A-* keybindings (where I have bound the option key to A) for:
;; - editing (inserting unicode characters, the sort of things that ctrl and meta are
;;   often used for, etc.)
;; - things that I might want to do multiple times in succession (because it's more
;;   convenient to do that with a modifier key than with a prefix key)
;;   - Update: but for these commands that I want to do multiple times in succession, I
;;     can also use the 'repeat' command (which I'm binding to A-h), so I can do it the
;;     first time using some other command that's hard to repeat, then repeat it with A-h.
;; - But I'm trying not to have too many A-* keybindings, because they involve an
;;   unergonomic stretch of my thumb.
;;
;; I'm using H-* keybindings for commands where I want a simple, one-key binding (grep,
;; switching frames, etc.). I feel like the most ergonomic way to access H, though, is to
;; define an easy keybinding that acts like H (currently M-h).
;;
;; I'm also defining some C-x * keybindings (particularly things that are similar to other
;; C-x keybindings) as well as C-c * keybindings (so far for mode-specific things, defined
;; elsewhere).
;;
;; I also have some super (s-*) keybindings. These are generally used to group together
;; similar commands. For example, s-w groups together some window-related commands. (As
;; with hyper, I currently activate super key bindings through a separate key binding,
;; C-h.)
;;
;; In the future I could see adding C-return or M-return as a prefix for some commands
;; (though note that these conflict with default key bindings in org mode).

(require 'macros)

;; Use M-h for hyper, C-h for super. I had been using home and esc for hyper and end for
;; super, but I think these reaches were contributing to thumb pain, so I'm trying these
;; alternate bindings instead.
;;
;; First I need to reassign what is typically assigned to M-h:
(global-unset-key (kbd "M-h")) ;; M-h is mark-paragraph: I'll put it on M-i
(global-unset-key (kbd "M-i")) ;; M-i is tab-to-tab-stop, which I don't find useful
(global-set-key (kbd "M-i") 'mark-paragraph)
;; and stop using C-h as help; can still get help with f1:
(global-unset-key (kbd "C-h"))
(setq help-char nil)
;; Now set M-h and C-h to do what I want:
(define-key function-key-map (kbd "M-h") 'event-apply-hyper-modifier)
(define-key function-key-map (kbd "C-h") 'event-apply-super-modifier)

;; Allow use of standard Mac keybindings to copy & paste. This makes it easier to copy
;; text back and forth between emacs and other applications.
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'easy-kill)
(global-unset-key (kbd "M-v"))
(global-set-key (kbd "M-v") 'yank)
;; Now that I have rebound M-v, I'll just use PageUp (i.e., <prior>) for
;; scroll-down-command; I like that this makes it easier to apply various variants (see
;; below).

;; Use dwim versions of upcase and downcase
(global-set-key [remap upcase-word] 'upcase-dwim)
(global-set-key [remap downcase-word] 'downcase-dwim)
;; Above, I rebound M-c to easy-kill. By default, M-c is bound to capitalize-word; I'll
;; instead use M-U (similar to M-u, upcase-word, which I now use for upcase-dwim).
(global-set-key (kbd "M-U") 'capitalize-dwim)

;; Use a key binding for cycling frames that mimics what I use elsewhere on my Mac.
(global-unset-key (kbd "C-'"))
(require 'org)
(define-key org-mode-map (kbd "C-'") nil)
(global-unset-key (kbd "C-\""))
(global-set-key (kbd "C-'") 'other-frame)
(global-set-key (kbd "C-\"") 'my-other-frame-reverse)

;; This isn't mnemonic, but it's easy to press repeatedly.
(global-set-key (kbd "A-h") 'repeat)

;; Make an easier way to return to the last place in the buffer (easier than C-u C-SPC)
;; This binding (C-,) is similar to the M-, binding that pops from an xref location
(require 'flyspell)
(require 'org)
(global-unset-key (kbd "C-,"))
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key org-mode-map (kbd "C-,") nil)
(global-set-key (kbd "C-,") 'my-pop-local-mark-ring)

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
;; I have bound M-v to yank. This is useful in isearch, too - and I want it to yank into
;; the search string, not the buffer (just like C-y yanks into the search string):
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)

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
(global-set-key (kbd "C-M-g") 'avy-goto-word-or-subword-1)

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

;; The following two are similar, but the crux version puts the cursor on the new line
;; (above), whereas my version does not.
(global-set-key (kbd "A-M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "A-o") 'my-insert-line-above)

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
(global-set-key (kbd "s-c e") 'my-flycheck-display-error-at-point)
(global-set-key (kbd "s-c h") 'my-flycheck-enable-highlighting)
(global-set-key (kbd "s-c H") 'my-flycheck-disable-highlighting)
(global-set-key (kbd "s-c l") 'flycheck-list-errors)
(global-set-key (kbd "s-c n") 'my-flycheck-next-error)
(global-set-key (kbd "s-c p") 'my-flycheck-previous-error)

;; the following will turn on hi-lock mode; to unhighlight just one, use C-x w r or M-s h u
(global-set-key (kbd "H-h") 'highlight-symbol-at-point)
(global-set-key (kbd "H-H") 'unhighlight-all-in-buffer)

;; bury buffer is a convenient way to remove a buffer from the tab line of one frame
;; without completely killing the buffer
(global-set-key (kbd "H-w") 'bury-buffer)

;; rename-uniquely is especially helpful in grep buffers
(global-set-key (kbd "H-u") 'rename-uniquely)

(global-set-key (kbd "H-b") 'my-goto-base-buffer)
(global-set-key (kbd "H-B") 'my-kill-buffer-and-goto-base-buffer)

;; I'm thinking I'll try to use jump-to-register more for going to some important place in
;; a buffer. But "C-x r j" is a lot to type, especially followed by one more character for
;; the register name. So I'm going to try an easier key binding to encourage myself to use
;; this easily. (I also thought about using something like M-g j, but I'd really like to
;; keep the number of keys to a minimum to make it easier to then press the register's
;; named key: I find it hard to type a bunch of random letters in a row.)
(global-set-key (kbd "H-j") 'jump-to-register)
;; and I guess I'll use H-J for its complement
(global-set-key (kbd "H-J") 'point-to-register)

(global-set-key (kbd "s-2 1") 'my-line-spacing-single)
(global-set-key (kbd "s-2 2") 'my-line-spacing-one-point-two-five)
(global-set-key (kbd "s-2 3") 'my-line-spacing-one-point-five)
(global-set-key (kbd "s-2 4") 'my-line-spacing-double)

;; these are convenient ways to cycle through tabs in the tab line
(global-set-key (kbd "<M-A-left>") 'previous-buffer)
(global-set-key (kbd "<M-A-right>") 'next-buffer)
;; these are slightly more ergonomic key bindings for the frequently-used forward-paragraph and backward-paragraph
(global-set-key (kbd "<M-A-up>") 'backward-paragraph)
(global-set-key (kbd "<M-A-down>") 'forward-paragraph)
;; But it's also helpful to have a right-hand-only key binding for forward and backward
;; paragraph, for when I'm browsing through code and don't want to bother having my left
;; hand on the keyboard; I'm using C-f and C-b for these because of their nice placement
;; in the Dvorak layout and because I use arrow keys rather than C-f and C-b for movement.
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-b") 'backward-paragraph)
(global-set-key (kbd "C-f") 'forward-paragraph)
;; it might be helpful to have a way to move up and down faster
(global-set-key (kbd "<A-up>") 'my-previous-line-3)
(global-set-key (kbd "<A-down>") 'my-next-line-3)
(require 'scroll-lock)
(define-key scroll-lock-mode-map (kbd "<A-up>") 'my-scroll-lock-previous-line-3)
(define-key scroll-lock-mode-map (kbd "<A-down>") 'my-scroll-lock-next-line-3)
;; And these are good keybindings for move-text; note that these bindings are shadowed in
;; org mode, but I feel that's actually a good thing: I generally don't want to move
;; single lines in org mode, and the effect is similar: in org mode, these bindings move a
;; subtree up / down.
(require 'move-text)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

(global-set-key (kbd "M-A-f") 'forward-symbol)
(global-set-key (kbd "M-A-b") 'my-backward-symbol)
(global-set-key (kbd "A-f") 'my-forward-to-whitespace)
(global-set-key (kbd "A-b") 'my-backward-to-whitespace)

;; helpful when a line is wrapped
(global-set-key (kbd "A-n") 'next-logical-line)
(global-set-key (kbd "A-p") 'previous-logical-line)

;; I have rebound M-v to paste. For the most part I'll use page up / page down, but in
;; case I want to use 'v' for paging (which is sometimes more convenient), I'm adding this
;; key binding:
(global-set-key (kbd "C-S-v") 'scroll-down-command)

;; (2021-08-29) I may want to make plain page up / down scroll by 3, requiring M to scroll
;; by a whole page: it may be that I more commonly scroll by 3 than a whole page... I want
;; to give that some thought.
(global-set-key (kbd "<M-next>") 'scroll-up-by-3)
(global-set-key (kbd "<M-prior>") 'scroll-down-by-3)
(global-set-key (kbd "<A-M-next>") 'scroll-up-by-10)
(global-set-key (kbd "<A-M-prior>") 'scroll-down-by-10)

(global-set-key (kbd "<S-next>") 'scroll-other-window)
(global-set-key (kbd "<S-prior>") 'scroll-other-window-down)
(global-set-key (kbd "<S-M-next>") 'my-scroll-other-window-up-by-3)
(global-set-key (kbd "<S-M-prior>") 'my-scroll-other-window-down-by-3)
(global-set-key (kbd "<S-A-M-next>") 'my-scroll-other-window-up-by-10)
(global-set-key (kbd "<S-A-M-prior>") 'my-scroll-other-window-down-by-10)

;; (2021-08-31) I'd like to consider replacing these bindings with something more
;; ergonomic, avoiding Alt when possible
(global-set-key (kbd "A-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "A-t") 'indent-relative)
(global-set-key (kbd "A-SPC") 'cycle-spacing)

;; I'm using s-x for enabling / disabling modes
;; (2021-08-10) Maybe change this to some symbol key, or maybe s-SPC or s-RET
(global-unset-key (kbd "s-x"))
;; This is useful to disable auto-triggering of company in buffers where that causes a big slowdown (e.g., case.py)
(global-set-key (kbd "s-x c") 'my-toggle-company-idle-delay)
(global-set-key (kbd "s-x f") 'auto-fill-mode)
(global-set-key (kbd "s-x i") 'highlight-indent-guides-mode)
(global-set-key (kbd "s-x I") 'my-toggle-highlight-indent-guides-responsive)
(global-set-key (kbd "s-x s") 'scroll-lock-mode)
(global-set-key (kbd "s-x v") 'view-mode)
;; This is helpful with files that magit opens from a different revision:
;; which-function-mode doesn't get enabled properly in these files.
(global-set-key (kbd "s-x w") 'my-enable-which-function-mode)

(global-set-key (kbd "H-i") 'counsel-imenu)
(global-set-key (kbd "H-I") 'my-counsel-imenu-current-pos)
(global-set-key (kbd "H-M-i") 'lsp-ui-imenu)
(global-set-key (kbd "H-A-i") 'imenu-anywhere)

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
(global-set-key (kbd "s-w -") 'text-scale-decrease)
(global-set-key (kbd "s-w +") 'text-scale-increase)
(global-set-key (kbd "s-w z") 'text-scale-adjust)

;; Some workarounds for issues (at least with emacsformacosx)

;; Sometimes the screen goes mostly blank and needs redrawing
(global-set-key (kbd "H-r") 'redraw-display)

;; Sometimes scroll bars disappear when resizing a frame; this function fixes them; note
;; that we use the same modifiers as for Divvy, since this happens after using Divvy
(global-set-key (kbd "A-C-M-s") 'fix-scroll-bars)
