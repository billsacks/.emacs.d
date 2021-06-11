(require 'counsel)
(require 'which-func)

;; same as counsel-imenu, but default to the current location (the only change from
;; counsel-imenu is the 'preselect' line below)
(defun my-counsel-imenu-current-pos ()
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (ivy-read "imenu items: " (counsel--imenu-candidates)
            :preselect (which-function)
            :require-match t
            :action #'counsel-imenu-action
            :keymap counsel-imenu-map
            :history 'counsel-imenu-history
            :caller 'counsel-imenu))
