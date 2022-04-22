;; General keybinding notes:
;;
;; I'm using A-* keybindings (where I have bound the option key to A) for:
;; - editing (inserting unicode characters, the sort of things that ctrl and meta are
;;   often used for, etc.)
;; - things that I might want to do multiple times in succession (because it's more
;;   convenient to do that with a modifier key than with a prefix key)
;;   - Update: but for these commands that I want to do multiple times in succession, I
;;     can also use the 'repeat' command (which I'm binding to C-M-c), so I can do it the
;;     first time using some other command that's hard to repeat, then repeat it with C-M-c.
;; - But I'm trying not to have too many A-* keybindings, because they involve an
;;   unergonomic stretch of my thumb.
;;
;; I'm using H-* keybindings for commands where I want a simple, one-key binding (grep,
;; switching frames, etc.). I feel like the most ergonomic way to access H, though, is to
;; define an easy keybinding that acts like H (currently M-c).
;;
;; I'm also defining some C-x * keybindings (particularly things that are similar to other
;; C-x keybindings) as well as C-c * keybindings (so far for mode-specific things, defined
;; elsewhere).
;;
;; I also have some super (s-*) keybindings. These are generally used to group together
;; similar commands. For example, s-w groups together some window-related commands. I am
;; currently using <end> as super.
;;
;; In the future I could see adding C-return or M-return as a prefix for some commands
;; (though note that these conflict with default key bindings in org mode).

(require 'macros)

;; Function to remap the binding for a key in a local map (from https://emacs.stackexchange.com/questions/58317/how-to-change-keybinding-for-all-similar-functions-across-all-modes)
;; This can be run for all modes by adding a specific function to after-change-major-mode-hook
(defun my-remap-in-local-bindings (orig_keys new_keys)
  "Remap whatever command is locally bound to orig_keys to new_keys

orig_keys and new_keys are strings like 'M-h' that can be read by the kbd function"
  (let* ((map (current-local-map))
         ;; in the following, I'm not sure if it's right to have the 't' final argument to lookup-key
         (local-binding (lookup-key map (kbd orig_keys) t)))
    (when local-binding
      (define-key map (kbd orig_keys) nil)
      (define-key map (kbd new_keys) local-binding))))

;; Use M-c for hyper and <end> for super
;; (M-c is pretty easy and has a nice parallel with the C-c bindings)
(global-unset-key (kbd "M-c")) ;; first reassign M-c to M-u
(global-set-key (kbd "M-u") 'capitalize-word)
(global-set-key (kbd "M-U") 'upcase-dwim) ;; then use M-U instead of M-u for upcase-dwim
(define-key isearch-mode-map (kbd "M-c") nil) ;; this toggles case-sensitivity, which can also be done via M-s c
(define-key function-key-map (kbd "M-c") 'event-apply-hyper-modifier)
(global-unset-key (kbd "<end>"))
(define-key function-key-map (kbd "<end>") 'event-apply-super-modifier)

;; Slightly more ergonomic binding for the common save-buffer (C-x C-s involves an awkward
;; stretch if I try to use Ctrl on the same hand for both the x and s with Dvorak)
(global-set-key (kbd "H-s") 'save-buffer)

;; Use dwim versions of upcase and downcase
(global-set-key [remap upcase-word] 'upcase-dwim)
(global-set-key [remap downcase-word] 'downcase-dwim)

;; Use a key binding for cycling frames that mimics what I use elsewhere on my Mac.
(global-unset-key (kbd "C-'"))
(require 'org)
(define-key org-mode-map (kbd "C-'") nil)
(global-unset-key (kbd "C-\""))
(global-set-key (kbd "C-'") 'other-frame)
(global-set-key (kbd "C-\"") 'my-other-frame-reverse)

;; Add a binding for repeating the last command that is easy to press repeatedly.
;;
;; This binding is vaguely similar to the use of C-c and M-c for custom bindings. The
;; important thing is that it's easy to press repeatedly.
;;
;; But first rebind what is normally bound to C-M-c: exit-recursive-edit (this is used for
;; things like ediff-regions; I'm putting it on A-g since it seems somewhat similar to
;; C-g).
(global-unset-key (kbd "C-M-c"))
(global-set-key (kbd "A-g") 'exit-recursive-edit)
(global-set-key (kbd "C-M-c") 'repeat)

;; Make an easier way to return to the last place in the buffer (easier than C-u C-SPC)
;; This binding (C-,) is similar to the M-, binding that pops from an xref location
(require 'flyspell)
(require 'org)
(global-unset-key (kbd "C-,"))
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key org-mode-map (kbd "C-,") nil)
(global-set-key (kbd "C-,") 'my-pop-local-mark-ring)

(global-set-key (kbd "C-S-l") 'my-recenter-to-bottom)
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
;; results, it's awkward to hit C-l. This Shift-Space binding is easier.
(define-key isearch-mode-map (kbd "<S-SPC>")
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
(global-set-key (kbd "C-M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "C-M-S-o") 'my-insert-line-above)

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
(global-set-key (kbd "A-w") 'bury-buffer)

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

;; These are convenient ways to cycle through tabs in the tab line. I chose these based on
;; ease of pressing and the fact that they seem okay to replace the default bindings
;; (rather than anything mnemonic). Also, I like the symmetry with windmove: now both
;; changing windows and prev/next buffer uses Ctrl on the left and middle finger / pinky
;; on the right (with windmove using Ctrl-left/right arrows). I don't need C-p because I
;; use the up arrow for this; C-. seems not to be bound by default, but with flyspell
;; loaded, it autocorrects the current word; but that's also bound to C-M-i, and there is
;; also C-; which seems similar.
;;
;; If these bindings become problematic (due to conflicts with other bindings), I could
;; consider changing them to C-M-left/right, now that I can do the C-M combo entirely on
;; my right hand.
(define-key flyspell-mode-map (kbd "C-.") nil)
(global-set-key (kbd "C-.") 'previous-buffer)
(global-set-key (kbd "C-p") 'next-buffer)

;; These are more ergonomic key bindings for the frequently-used forward-paragraph and
;; backward-paragraph. It's helpful to have these be one-handed bindings, for when I'm
;; browsing through code and don't want to bother having my left hand on the keyboard; I'm
;; using C-f and C-b for these because of their nice placement in the Dvorak layout and
;; because I use arrow keys rather than C-f and C-b for movement.
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-b") 'backward-paragraph)
(global-set-key (kbd "C-f") 'forward-paragraph)
;; Also use C-b and C-f in other modes that rebind M-{ and M-} (e.g., grep for navigating between files)
(defun my-remap-M-curly-open-to-C-b ()
  "Remap whatever command is locally bound to `M-{' to `C-b'"
  (my-remap-in-local-bindings "M-{" "C-b"))
(add-hook 'after-change-major-mode-hook #'my-remap-M-curly-open-to-C-b)
(defun my-remap-M-curly-close-to-C-f ()
  "Remap whatever command is locally bound to `M-}' to `C-f'"
  (my-remap-in-local-bindings "M-}" "C-f"))
(add-hook 'after-change-major-mode-hook #'my-remap-M-curly-close-to-C-f)

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

(global-set-key (kbd "A-M-f") 'forward-symbol)
(global-set-key (kbd "A-M-b") 'my-backward-symbol)
(global-set-key (kbd "A-f") 'my-forward-to-whitespace)
(global-set-key (kbd "A-b") 'my-backward-to-whitespace)

;; helpful when a line is wrapped, and in visual-line-mode
(global-set-key (kbd "<A-M-down>") 'next-logical-line)
(global-set-key (kbd "<A-M-up>") 'previous-logical-line)
;; the following are meant for when we're in visual-line-mode, as a way to force these
;; commands to work on logical lines (since C-a and C-e are rebound in visual-line-mode)
;; (normally C-a and C-e send move-beginning-of-line and move-end-of-line; these commands
;; are slightly different, but I'm using these versions because it appears that
;; visual-line-mode rebinds anything that is bound to move-beginning-of-line and
;; move-end-of-line):
(global-set-key (kbd "A-M-a") 'beginning-of-line)
(global-set-key (kbd "A-M-e") 'end-of-line)

;; I am moving some key bindings so that I can (1) (the primary motivation) maintain the
;; use of Cmd-v for pasting (I am especially inspired to do this so that I can continue to
;; use Cmd-v in iterm2 with the physical command key, despite swapping Cmd and Opt in
;; iterm2 preferences); and (2) have more ergonomic bindings for the common scroll
;; commands (since 't' is an easier key than 'v'):
;; - I am putting yank on M-v (in addition to the normal C-y)
;; - I am putting the scrolling commands on t instead of v
;; - I am putting the transpose bindings on z instead of t (mnemonic: "transpoZe")
;; - I am moving the bindings on z to elsewhere, or removing them
;;
;; First move the important bindings on z to elsewhere:
;;
;; replace zap-to-char functionality with the more powerful zop-to-char; use H-w and H-W
;; (instead of M-z, which we want for other purposes) to mimic C-w and M-w functionality
(global-set-key (kbd "H-w") 'zop-up-to-char)
(global-set-key (kbd "H-W") 'zop-to-char)
;;
;; Now move the transpose bindings to z instead of t:
(global-set-key (kbd "C-z") 'transpose-chars)
(defun my-remap-C-t-to-C-z ()
  "Remap whatever command is locally bound to `C-t' to `C-z'"
  (my-remap-in-local-bindings "C-t" "C-z"))
(add-hook 'after-change-major-mode-hook #'my-remap-C-t-to-C-z)
(global-set-key (kbd "M-z") 'transpose-words)
(defun my-remap-M-t-to-M-z ()
  "Remap whatever command is locally bound to `M-t' to `M-z'"
  (my-remap-in-local-bindings "M-t" "M-z"))
(add-hook 'after-change-major-mode-hook #'my-remap-M-t-to-M-z)
(global-set-key (kbd "C-M-z") 'transpose-sexps)
(defun my-remap-C-M-t-to-C-M-z ()
  "Remap whatever command is locally bound to `C-M-t' to `C-M-z'"
  (my-remap-in-local-bindings "C-M-t" "C-M-z"))
(add-hook 'after-change-major-mode-hook #'my-remap-C-M-t-to-C-M-z)
;;
;; Now put the scrolling commands on t instead of v; also switch the meaning of C and M:
;; because of my positioning of Meta on my Kinesis keyboard, it is both more mnemonic (up
;; = up, down = down) and more ergonomic to use M for scroll-up and C for scroll-down.
(global-set-key (kbd "M-t") 'scroll-up-command)
(defun my-remap-C-v-to-M-t ()
  "Remap whatever command is locally bound to `C-v' to `M-t'"
  (my-remap-in-local-bindings "C-v" "M-t"))
(add-hook 'after-change-major-mode-hook #'my-remap-C-v-to-M-t)
(global-set-key (kbd "C-t") 'scroll-down-command)
(defun my-remap-M-v-to-C-t ()
  "Remap whatever command is locally bound to `M-v' to `C-t'"
  (my-remap-in-local-bindings "M-v" "C-t"))
(add-hook 'after-change-major-mode-hook #'my-remap-M-v-to-C-t)
(global-set-key (kbd "A-M-t") 'scroll-other-window)
(global-set-key (kbd "C-M-t") 'scroll-other-window-down)
;;
;; Finally, M-v is free to use as yank
(global-set-key (kbd "M-v") 'yank)
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)

(global-set-key (kbd "<S-next>") 'scroll-other-window)
(global-set-key (kbd "<S-prior>") 'scroll-other-window-down)

;; Use C/M-h for scrolling by 3; note that these bindings mimic the bindings on C/M-t
;; (i.e., what is C/M-v by default, but I have moved to C/M-t).
;;
;; First I need to reassign what is typically assigned to C-h, M-h and C-M-h; I am using
;; 'i' for highlighting-related bindings that used to be on 'h':
(global-unset-key (kbd "C-h")) ;; stop using C-h as help; can still get help with f1
(global-unset-key (kbd "M-h")) ;; M-h is mark-paragraph: I'll put it on M-i
(global-unset-key (kbd "M-i")) ;; M-i is tab-to-tab-stop, which I don't find useful
(global-set-key (kbd "M-i") 'mark-paragraph)
(defun my-remap-M-h-to-M-i ()
  "Remap whatever command is locally bound to `M-h' to `M-i'"
  (my-remap-in-local-bindings "M-h" "M-i"))
(add-hook 'after-change-major-mode-hook #'my-remap-M-h-to-M-i)
(global-unset-key (kbd "C-M-h")) ;; C-M-h is mark-defun; I'll put it on C-M-i
(define-key flyspell-mode-map (kbd "C-M-i") nil) ;; but first I need to assign flyspell's C-M-i (flyspell-auto-correct-word )to something else
(define-key flyspell-mode-map (kbd "C-M-;") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-M-i") 'mark-defun)
(defun my-remap-C-M-h-to-C-M-i ()
  "Remap whatever command is locally bound to `C-M-h' to `C-M-i'"
  (my-remap-in-local-bindings "C-M-h" "C-M-i"))
(add-hook 'after-change-major-mode-hook #'my-remap-C-M-h-to-C-M-i)
;; Now, finally, I can do the assignments for scrolling
(global-set-key (kbd "M-h") 'scroll-up-by-3)
(global-set-key (kbd "C-h") 'scroll-down-by-3)
(global-set-key (kbd "A-M-h") 'my-scroll-other-window-up-by-3)
(global-set-key (kbd "C-M-h") 'my-scroll-other-window-down-by-3)
;; I also want C-h to scroll-down-by-3 during isearch, rather than pulling up help; I think
;; another way to accomplish this would be to do a global setting of the help char to nil,
;; but I think I ran into other issues when I tried to do that (specifically, which-key
;; stopped working). If I'm finding that I need to do the following for a number of modes,
;; another workaround could be defining the help char to something I would never use
;; (e.g., "`").
(define-key isearch-mode-map (kbd "C-h") 'scroll-down-by-3)

;; This can be useful during one-handed scrolling
(global-set-key (kbd "<C-next>") 'scroll-up-by-3)
(global-set-key (kbd "<C-prior>") 'scroll-down-by-3)

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
;; winner-undo and redo mirror standard C-/ and C-? for undo/redo
(global-set-key (kbd "s-w /") 'winner-undo)
(global-set-key (kbd "s-w ?") 'winner-redo)
(global-set-key (kbd "s-w -") 'text-scale-decrease)
(global-set-key (kbd "s-w +") 'text-scale-increase)
(global-set-key (kbd "s-w z") 'text-scale-adjust)
;; like Divvy, but with the added bonus of creating or removing window splits
(global-set-key (kbd "s-w <left>") 'my-make-frame-half-left)
(global-set-key (kbd "s-w <right>") 'my-make-frame-half-right)
(global-set-key (kbd "s-w SPC") 'my-make-frame-full-and-split)
;; transposing frame
(global-set-key (kbd "s-w t") 'transpose-frame)
(global-set-key (kbd "s-w T") 'my-make-frame-full-and-transpose)

;; Some workarounds for issues (at least with emacsformacosx)

;; Sometimes the screen goes mostly blank and needs redrawing
(global-set-key (kbd "H-r") 'redraw-display)

;; Sometimes scroll bars disappear when resizing a frame; this function fixes them; note
;; that we use the same modifiers as for Divvy, since this happens after using Divvy
(global-set-key (kbd "A-C-M-s") 'fix-scroll-bars)
