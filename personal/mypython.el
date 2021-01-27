(require 'electric)
(require 'prelude-programming)
(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)

;; from prelude-python.el
(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun my-python-mode-defaults ()
  "Defaults for Python programming."
  ;; should we include eldoc-mode? it's included in prelude-python.el, but that may be
  ;; because it's connected to anaconda

  ;; the following is all from prelude-python.el
  (subword-mode +1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local))

(setq my-python-mode-hook 'my-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'my-python-mode-hook)))

(defun python-flycheck-setup()
  (flycheck-select-checker 'python-pylint))
(add-hook 'lsp-pyls-after-open-hook #'python-flycheck-setup)
