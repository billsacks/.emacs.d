(require 'flycheck)

;; Redefine this flycheck function: This is a workaround for issues with the continuation
;; character in the margin:
;; - The default character is too tall, leading to lines shifting around when it's shown
;; - The continuation character appears next to the regular one, so you can't see the regular one
;;
;; This workaround leads to not showing a continuation character at all. This feels okay,
;; because I don't think there's a lot of value in showing all of these continuation lines
;; of an error. An alternative would be to hard-code some other continuation character
;; here, such as using the normal margin character.
(defun flycheck-error-level-margin-continuation-spec (level)
  (flycheck-make-margin-spec "" 'default))

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

;; The following functions are useful given that I have set the default flycheck display
;; errors function to "ignore": this function lets us manually display errors at point
(defun my-flycheck-display-error-at-point ()
  "Display current flycheck error in the minibuffer"
  (interactive)
  (let ((flycheck-display-errors-function #'flycheck-display-error-messages))
    (flycheck-display-error-at-point)))
(defun my-flycheck-next-error ()
  "Go to next flycheck error and display it in the minibuffer"
  (interactive)
  (flycheck-next-error)
  (my-flycheck-display-error-at-point))
(defun my-flycheck-previous-error ()
  "Go to previous flycheck error and display it in the minibuffer"
  (interactive)
  (flycheck-previous-error)
  (my-flycheck-display-error-at-point))

(defun my-flycheck-enable-highlighting ()
  "Set flycheck highlighting mode to symbols in current buffer"
  (interactive)
  (setq-local flycheck-highlighting-mode 'symbols)
  ;; need to rerun flycheck for change to take effect
  (flycheck-buffer))
(defun my-flycheck-disable-highlighting ()
  "Turn off flycheck inline highlighting in current buffer"
  (interactive)
  (setq-local flycheck-highlighting-mode nil)
  ;; need to rerun flycheck for change to take effect
  (flycheck-buffer))

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
