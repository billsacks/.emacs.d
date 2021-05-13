(require 'flycheck)

;; My version of flycheck-mode-line-status-text. See comments for what I'm changing.
(defun my-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running " +| +| +")
                (`errored "!")
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (format "%2s|%2s|%2s" (or .error '-) (or .warning '-) (or .info '-))
                   ))
                (`interrupted ".")
                (`suspicious "?"))))
    (concat " " flycheck-mode-line-prefix text)))

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
