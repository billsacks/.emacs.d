(require 'org)
(setq org-todo-keywords '(
                          (sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
                          ))

(setq org-todo-keyword-faces
      '(("TODO" . "red") ("DONE" . "ForestGreen")
        ("CANCELED" . "blue")))
