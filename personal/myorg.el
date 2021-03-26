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
