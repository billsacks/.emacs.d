(require 'macros)

;; This allows the end key to be a sticky modifier, which is helpful since option isn't a
;; very ergonomic modifier
(define-key key-translation-map (kbd "<end>") #'event-apply-super-modifier)
;; I kind of want to define home to be something, too, but I'm not sure what would be most
;; useful

;; Swiper key-bindings
(global-set-key (kbd "s-s") 'swiper)

;; Avy key-bindings
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
;; Note that you can use this for goto-line by entering numbers, so it's safe to override
;; the built-in M-g g for goto-line
(global-set-key (kbd "M-g g") 'avy-goto-line)

;; Some other convenient shortcuts
(global-set-key (kbd "<s-down>") (kbd "C-u 3 C-v"))
(global-set-key (kbd "<s-up>") (kbd "C-u 3 M-v"))
(global-set-key (kbd "C-c c") 'copy-current-line)
(define-key prelude-mode-map (kbd "C-c f") nil)
(global-set-key (kbd "C-c f") 'auto-fill-mode)
(define-key prelude-mode-map (kbd "C-c t") nil)
(global-set-key (kbd "C-c t") 'indent-relative)
