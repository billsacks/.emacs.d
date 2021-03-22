(require 'org)
(setq org-todo-keywords '(
                          (sequence "TODO" "|" "DONE" "CANCELED")
                          ))

(setq org-todo-keyword-faces
      '(("TODO" . "red") ("DONE" . "ForestGreen")
        ("CANCELED" . "blue")))
