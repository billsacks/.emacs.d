(require 'macros)

;; Swiper key-bindings
(global-set-key (kbd "C-s") 'swiper-isearch)
;; The point of the backward search is two-fold:
;; (1) because it's symmetrical with forward search
;; (2) it can be useful to go to the start of the search term rather
;;     than the end of the search term
(global-set-key (kbd "C-r") 'swiper-isearch-backward)

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
