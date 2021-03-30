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
