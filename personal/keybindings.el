(require 'macros)

;; With command acting as meta, escape is more useful as super rather than meta (since
;; option isn't a very ergonomic modifier). Also make "home" give super because home isn't
;; very useful in emacs, and this gives symmetry.
(define-key key-translation-map (kbd "ESC") #'event-apply-super-modifier)
(define-key key-translation-map (kbd "<home>") #'event-apply-super-modifier)

;; Swiper key-bindings
(global-set-key (kbd "C-S-s") 'swiper)
(global-set-key (kbd "C-S-r") 'swiper-backward)

;; Avy key-bindings
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
;; Note that you can use this for goto-line by entering numbers, so it's safe to override
;; the built-in M-g g for goto-line
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

;; Some other convenient shortcuts
(global-set-key (kbd "<s-down>") (kbd "C-u 3 C-v"))
(global-set-key (kbd "<s-up>") (kbd "C-u 3 M-v"))
(global-set-key (kbd "C-c c") 'copy-current-line)
(define-key prelude-mode-map (kbd "C-c f") nil)
(global-set-key (kbd "C-c f") 'auto-fill-mode)
(define-key prelude-mode-map (kbd "C-c t") nil)
(global-set-key (kbd "C-c t") 'indent-relative)
