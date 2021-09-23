(require 'nxml-mode)
;; Follow my remapping from M-h to M-i in the global map
(define-key nxml-mode-map (kbd "M-h") nil)
(define-key nxml-mode-map (kbd "M-i") 'nxml-mark-paragraph)
