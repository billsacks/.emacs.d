(require 'macros)

;; With command acting as meta, escape is more useful as super rather than meta (since
;; option isn't a very ergonomic modifier). But keep ability to get escape with
;; shift-escape if I really need it. Also make "home" give super because home isn't very
;; useful in emacs, and this gives symmetry. Make "end" give hyper because otherwise we
;; don't have a way to give hyper (though I'm not sure if we'll use it for anything).
(define-key key-translation-map (kbd "ESC") #'event-apply-super-modifier)
(define-key key-translation-map (kbd "S-<escape>") (kbd "ESC"))
(define-key key-translation-map (kbd "<home>") #'event-apply-super-modifier)
(define-key key-translation-map (kbd "<end>") #'event-apply-hyper-modifier)

(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)

;; ivy/swiper key-bindings
(global-set-key (kbd "C-S-s") 'swiper)
(global-set-key (kbd "C-S-r") 'swiper-backward)
;; this is useful when you don't want to use the completion candidate, but just want to
;; use the current input, such as inputting file name patterns and directories for rgrep;
;; it is bound to C-M-j by default, but C-Return is easier
(require 'ivy)
(define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

;; Avy key-bindings
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
;; Note that you can use this for goto-line by entering numbers, so it's safe to override
;; the built-in M-g g for goto-line
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

;; Shortcuts to some Projectile things
(global-set-key (kbd "A-g") 'projectile-grep)

;; Some other convenient shortcuts

(global-set-key (kbd "A-G") 'vc-git-grep)

;; bury buffer is a convenient way to remove a buffer from the tab line of one frame
;; without completely killing the buffer
(global-set-key (kbd "C-x w") 'bury-buffer)

;; rename-uniquely is especially helpful in grep buffers
(global-set-key (kbd "A-u") 'rename-uniquely)

;; these are convenient ways to cycle through tabs in the tab line
(global-set-key (kbd "<M-S-left>") 'previous-buffer)
(global-set-key (kbd "<M-S-right>") 'next-buffer)
;; but I need to then redefine them in org mode
(define-key org-mode-map (kbd "<M-S-left>") nil)
(define-key org-mode-map (kbd "<M-S-right>") nil)
(define-key org-mode-map (kbd "<A-left>") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "<A-right>") 'org-shiftmetaright)

(global-set-key (kbd "<A-down>") (kbd "C-u 3 C-v"))
(global-set-key (kbd "<A-up>") (kbd "C-u 3 M-v"))

(global-set-key (kbd "C-c c") 'copy-current-line)
(define-key prelude-mode-map (kbd "C-c f") nil)
(global-set-key (kbd "C-c f") 'auto-fill-mode)
(define-key prelude-mode-map (kbd "C-c t") nil)
(global-set-key (kbd "C-c t") 'indent-relative)

(global-set-key (kbd "A-i") 'counsel-imenu)
(global-set-key (kbd "A-I") 'lsp-ui-imenu)
(global-set-key (kbd "A-f") 'select-frame-by-name)

(global-set-key (kbd "A-m") 'move-buffer-to-other-window)

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
