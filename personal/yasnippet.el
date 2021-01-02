(require 'dropdown-list)
(require 'yasnippet)
(yas-global-mode 1)

;; To avoid conflicts with company mode, use S-SPC in place of tab for
;; yasnippet (and then use S-M-SPC where yasnippet used to use S-tab)
;;
;; (Note, though, that in text terminals, the shifted things may not
;; work (see
;; http://ergoemacs.org/emacs_manual/elisp/Other-Char-Bits.html).)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "S-SPC") yas-maybe-expand)
(eval-after-load 'yasnippet
  '(progn
     (define-key yas-keymap [(tab)]       nil)
     (define-key yas-keymap (kbd "TAB")   nil)
     (define-key yas-keymap [(shift tab)] nil)
     (define-key yas-keymap [backtab]     nil)
     (define-key yas-keymap (kbd "S-SPC") 'yas-next-field-or-maybe-expand)
     (define-key yas-keymap (kbd "S-M-SPC") 'yas-prev-field)))

;; If yasnippet is set to do any indentation (which typically is helpful), it
;; results in indented blank lines. Clean those up with this hook:
(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (whitespace-cleanup-region yas/snippet-beg
                                        yas/snippet-end)))
