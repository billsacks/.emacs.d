(require 'compile)

;; Use 'o' to open a compilation result, grep result, etc. in the same window as the
;; compilation-mode (or grep-mode or whatever) buffer
;;
;; from https://emacs.stackexchange.com/questions/33857/open-search-result-in-the-same-window
(defun my-compile-goto-error-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))

(defun my-compile-goto-next-error ()
  (interactive)
  (compilation-next-error 1)
  (compile-goto-error))
(defun my-compile-goto-previous-error ()
  (interactive)
  (compilation-previous-error 1)
  (compile-goto-error))

(defun my-compilation-mode-hook ()
  (local-set-key (kbd "<M-return>") #'my-compile-goto-error-same-window)
  (local-set-key (kbd "N") #'my-compile-goto-next-error)
  (local-set-key (kbd "P") #'my-compile-goto-previous-error)
  (hl-line-mode +1))
(add-hook 'compilation-mode-hook #'my-compilation-mode-hook)
