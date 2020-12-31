(global-set-key (kbd "C-s") 'swiper-isearch)
;; The point of the backward search is two-fold:
;; (1) because it's symmetrical with forward search
;; (2) it can be useful to go to the start of the search term rather
;;     than the end of the search term
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
