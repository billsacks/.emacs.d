;;; This has changes for both org itself and deft

(require 'org)
(require 'deft)
(setq org-todo-keywords '(
                          (sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
                          ))

(setq org-todo-keyword-faces
      '(("TODO" . "red") ("DONE" . "ForestGreen")
        ("CANCELED" . "blue")))

(global-set-key [remap org-set-tags-command] #'counsel-org-tag)
(define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-decrement-word)

;; org-display-outline-text doesn't show the last level; fix that with this function
(defun my-org-show-position-in-text () ;; display outline path of hierarchical headings
  (interactive)
  ;; first argument nil says do not prepend file name; second argument t says do append
  ;; current heading
  (org-display-outline-path nil t))

;; select the inline code block under cursor
(defun my-org-select-inline-code ()
  (interactive)
  (search-backward "~")
  (forward-char 1)
  (set-mark (point))
  (search-forward "~")
  (backward-char 1))

(defun my-deft-mode-hook ()
  (hl-line-mode +1))
(add-hook 'deft-mode-hook #'my-deft-mode-hook)

;; Allow exporting as Github-flavored markdown
(eval-after-load "org"
  '(require 'ox-gfm nil t))
