;; Clean up clutter in the mode line
(diminish 'yas-minor-mode)
(diminish 'company-mode)
(diminish 'whitespace-mode)
(diminish 'flyspell-mode)
(diminish 'smartparens-mode)
(diminish 'prelude-mode)
(diminish 'which-key-mode)
(diminish 'buffer-face-mode)
(diminish 'projectile-mode)
(diminish 'lsp-mode)
(diminish 'gcmh-mode)

;; this one needs to be diminished after being loaded
(add-hook 'ws-butler-mode-hook (lambda () (diminish 'ws-butler-mode)))
