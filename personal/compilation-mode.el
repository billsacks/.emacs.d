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
(defun my-compilation-mode-hook ()
  (local-set-key (kbd "o") #'my-compile-goto-error-same-window)
  (hl-line-mode +1))
(add-hook 'compilation-mode-hook #'my-compilation-mode-hook)
