(require 'dropdown-list)
(require 'yasnippet)
(yas-global-mode 1)

;; If yasnippet is set to do any indentation (which typically is helpful), it
;; results in indented blank lines. Clean those up with this hook:
(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (whitespace-cleanup-region yas/snippet-beg
                                        yas/snippet-end)))
