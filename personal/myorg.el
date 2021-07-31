;;; This has changes for both org itself and deft

(require 'org)
(require 'deft)
(setq org-todo-keywords '(
                          (sequence "TODO(t)" "PROG(p)" "HOLD(h)" "WAIT(w)" "DPND(n)" "|" "DONE(d)" "CANC(c)")
                          ))

(setq org-todo-keyword-faces
      '(("TODO" . "blue") ("PROG" . "red") ("HOLD" . "IndianRed1") ("WAIT" . "IndianRed1") ("DPND" . "IndianRed1")
        ("DONE" . "ForestGreen") ("CANC" . "tan1")))

(global-set-key [remap org-set-tags-command] #'counsel-org-tag)
(define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-decrement-word)

(define-key org-mode-map (kbd "C-c a") 'org-toggle-archive-tag)
(define-key org-mode-map (kbd "C-c A") 'org-archive-to-archive-sibling)

;; org-display-outline-text doesn't show the last level; fix that with this function
(defun my-org-show-position-in-text () ;; display outline path of hierarchical headings
  (interactive)
  ;; first argument nil says do not prepend file name; second argument t says do append
  ;; current heading
  (org-display-outline-path nil t))
;; mnemonic: o for "outline" (this is similar to org-get-outline-path)
(define-key org-mode-map (kbd "C-c o") 'my-org-show-position-in-text)

;; highlight the inline code block under cursor
(defun my-org-highlight-inline-code ()
  (interactive)
  (search-backward "~")
  (forward-char 1)
  (set-mark (point))
  (search-forward "~")
  (backward-char 1))
(define-key org-mode-map (kbd "C-c h") 'my-org-highlight-inline-code)

;; go to next visible todo
(defun my-org-next-visible-todo ()
  (interactive)
  (org-next-visible-heading 1)
  (while (and (not (org-entry-is-todo-p)) (not (eobp)))
    (org-next-visible-heading 1)))
(define-key org-mode-map (kbd "C-c C-S-n") 'my-org-next-visible-todo)
(defun my-org-forward-todo-same-level ()
  (interactive)
  (org-forward-heading-same-level 1)
  (while (and (not (org-entry-is-todo-p)) (not (eobp)))
    (org-forward-heading-same-level 1)))
(define-key org-mode-map (kbd "C-c C-S-f") 'my-org-forward-todo-same-level)

(defun my-org-insert-todo-heading (arg &optional force-heading)
  "Like org-insert-todo-heading but use the first state.

With no prefix args, this operates like org-insert-todo-heading
with one prefix arg.

With one prefix arg, force inserting at the end of the parent subtree
(like org-insert-todo-heading with two prefix args).

Note: the force-heading piece of this is untested."
  (interactive "P")
  (if (equal arg '(4))
      (org-insert-todo-heading '(16) force-heading)
    (org-insert-todo-heading '(4) force-heading)))

;; Quick ways to mark todos as done or canceled, possibly with archiving
(defun my-org-done ()
  "Change current heading's state to DONE"
  (interactive)
  (org-todo "DONE"))
(define-key org-mode-map (kbd "C-c d") 'my-org-done)
(defun my-org-done-and-archive ()
  "Change current heading's state to DONE and archive it"
  (interactive)
  (org-todo "DONE")
  (org-archive-to-archive-sibling))
(define-key org-mode-map (kbd "C-c D") 'my-org-done-and-archive)
(defun my-org-canceled ()
  "Change current heading's state to CANC"
  (interactive)
  (org-todo "CANC"))
(define-key org-mode-map (kbd "C-c c") 'my-org-canceled)
(defun my-org-canceled-and-archive ()
  "Change current heading's state to CANC and archive it"
  (interactive)
  (org-todo "CANC")
  (org-archive-to-archive-sibling))
(define-key org-mode-map (kbd "C-c C") 'my-org-canceled-and-archive)

(defun my-org-after-todo-state-change-hook ()
  "Function to run after todo state change"
  ;; When completing or canceling a todo, hide the entry.
  ;;
  ;; This is convenient so I don't need to go back up to the heading and hit tab.
  ;;
  ;; Instead of checking org-last-todo-state-is-todo, could check: (string= org-state
  ;; "DONE") and similarly for "CANC". But I like org-last-todo-state-is-todo (as long as
  ;; it works reliably) because I don't need to explicitly list all done states.
  (if (not org-last-todo-state-is-todo)
      (outline-hide-subtree))
  )
(add-hook 'org-after-todo-state-change-hook #'my-org-after-todo-state-change-hook)

;; adapted from https://emacs.stackexchange.com/questions/38537/move-org-mode-child-to-be-sibling-of-parent/38554
(defun my-org-move-subtree-down-and-promote ()
  "Move the current subtree to be a sibling of its parent, appearing below its parent"
  (interactive)
  (condition-case nil
      (while t (org-move-subtree-down))
    (error nil))
  (org-shiftmetaleft))
(define-key org-mode-map (kbd "C-c <left>") 'my-org-move-subtree-down-and-promote)

(defun my-deft-mode-hook ()
  (hl-line-mode +1))
(add-hook 'deft-mode-hook #'my-deft-mode-hook)

;; Allow exporting as Github-flavored markdown
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; This is available as C-c C-t, but I often do C-c t by accident, and since this is a
;; common thing I want to do, I'll make it a key binding
(define-key org-mode-map (kbd "C-c t") 'org-todo)

;; Hide / show body text. If there is a lot of body text before subheadings (children) it
;; can be helpful sometimes to hide the body text of an entry.
(define-key org-mode-map (kbd "C-c b") 'org-hide-entry)
(define-key org-mode-map (kbd "C-c B") 'org-show-entry)
