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

;; org-display-outline-text doesn't show the last level; fix that with this function from
;; https://emacs.stackexchange.com/questions/51054/org-mode-tell-paragraph-position-in-subtree
(defun my-org-show-position-in-text () ;; display outline path of hierarchical headings
  (interactive)
  (message (mapconcat #'identity (org-get-outline-path t) "/")))
