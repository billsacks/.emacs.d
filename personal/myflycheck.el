(require 'flycheck)

(defun my-flycheck-dash-for-zero (val)
  "Return value unless it's 0, in which case we return dash"
  (if (eq 0 val)
      '-
    val)
  )

(defun my-flycheck-error-is-type-p (err err-type)
  "Return t if the given err is of the given type (string), nil otherwise

A key piece of this involves counting errors of type
lsp-flycheck-ERRTYPE-* in addition to errors of type ERRTYPE.
"
  (let ((this-err-type (symbol-name (car err))))
    (or (string-equal err-type this-err-type)
        (string-prefix-p (format "lsp-flycheck-%s" err-type) this-err-type))
    ))

;; My version of flycheck-mode-line-status-text.
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
                 (let ((num_errors 0)
                       (num_warnings 0)
                       (num_infos 0))
                   (dolist (err (flycheck-count-errors flycheck-current-errors))
                     (cond ((my-flycheck-error-is-type-p err "error")
                            (setq num_errors (+ num_errors (cdr err))))
                           ((my-flycheck-error-is-type-p err "warning")
                            (setq num_warnings (+ num_warnings (cdr err))))
                           ((my-flycheck-error-is-type-p err "info")
                            (setq num_infos (+ num_infos (cdr err))))))
                   (format "%2s|%2s|%2s"
                           (my-flycheck-dash-for-zero num_errors)
                           (my-flycheck-dash-for-zero num_warnings)
                           (my-flycheck-dash-for-zero num_infos)
                           )
                   ))
                (`interrupted ".")
                (`suspicious "?"))))
    (concat " " flycheck-mode-line-prefix text)))

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
