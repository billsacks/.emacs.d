;; Swiper key-bindings
(global-set-key (kbd "C-s") 'swiper-isearch)
;; The point of the backward search is two-fold:
;; (1) because it's symmetrical with forward search
;; (2) it can be useful to go to the start of the search term rather
;;     than the end of the search term
(global-set-key (kbd "C-r") 'swiper-isearch-backward)

;; Avy key-bindings
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)

;; Some other convenient shortcuts
(global-set-key (kbd "<s-down>") (kbd "C-u 10 C-v"))
(global-set-key (kbd "<s-up>") (kbd "C-u 10 M-v"))
