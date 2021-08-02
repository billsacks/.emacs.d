;;; This has changes for both org itself and deft

(require 'org)
(require 'deft)
;; in org-todo-keywords, the first line gives a typical progression of states; following
;; lines give less common states
(setq org-todo-keywords '((sequence "TODO(t)" "SOON(s)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
                          (sequence "|" "CANC(c)")
                          (sequence "HOLD(h)" "WAIT(w)" "DPND(e)" "|")
                          ;; the following are for projects, both in my top-level projects
                          ;; file and in files for individual projects that are big enough
                          ;; that they are broken down into relatively large subprojects:
                          ;; - PR-1 is for ones I'm working on now (this week)
                          ;; - PR-2 is for relatively high priority, but not quite yet
                          ;; - PR-3 is normal priority
                          ;; - PR-4 is sometime in the more distant future
                          (sequence "PR-4(4)" "PR-3(3)" "PR-2(2)" "PR-1(1)" "|")))

(setq org-todo-keyword-faces
      '(("TODO" . "blue") ("SOON" . "IndianRed1") ("NEXT" . "red") ("PROG" . "red")
        ("HOLD" . "purple") ("WAIT" . "purple") ("DPND" . "purple")
        ("DONE" . "RosyBrown") ("CANC" . "RosyBrown")
        ;; For the PR-# states: I want a distinct color for PR-1, since that is the stuff
        ;; I should pay attention to this week. For the other three, I am using DarkBlue
        ;; for 2, LightBlue3 for 4, and an intermediate color (from
        ;; https://meyerweb.com/eric/tools/color-blend) for 3.
        ("PR-1" . (:inherit org-todo :foreground "DarkRed" :background "LightBlue"))
        ("PR-2" . (:inherit org-todo :foreground "DarkBlue" :background "LightBlue"))
        ("PR-3" . (:inherit org-todo :foreground "#4D60AC" :background "LightBlue"))
        ("PR-4" . (:inherit org-todo :foreground "LightBlue3" :background "LightBlue"))))

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

;; The following is useful for marking a bunch of things soon in quick succession: do this
;; once then do the key binding to repeat the last command as often as wanted
(defun my-org-soon-and-forward ()
  "Mark the current todo as soon, then go forward to the next todo at the same level"
  (interactive)
  (org-todo "SOON")
  (my-org-forward-todo-same-level))
(define-key org-mode-map (kbd "C-c s") 'my-org-soon-and-forward)

;; Quick ways to mark todos as done or canceled, possibly with archiving
(defun my-org-clear-priority ()
  "Clear the priority on the current todo, if there is one"
  (interactive)
  ;; adapted from https://www.reddit.com/r/orgmode/comments/8ih7md/delete_priority_on_task_repeat/
  (if (string-match org-priority-regexp (org-get-heading t t nil))
      (org-priority ?\ ))
  )
(defun my-org-done ()
  "Change current heading's state to DONE"
  (interactive)
  (org-todo "DONE")
  (my-org-clear-priority))
(define-key org-mode-map (kbd "C-c d") 'my-org-done)
(defun my-org-done-and-archive ()
  "Change current heading's state to DONE and archive it"
  (interactive)
  (my-org-done)
  (org-archive-to-archive-sibling))
(define-key org-mode-map (kbd "C-c D") 'my-org-done-and-archive)
(defun my-org-canceled ()
  "Change current heading's state to CANC"
  (interactive)
  (org-todo "CANC")
  (my-org-clear-priority))
(define-key org-mode-map (kbd "C-c c") 'my-org-canceled)
(defun my-org-canceled-and-archive ()
  "Change current heading's state to CANC and archive it"
  (interactive)
  (my-org-canceled)
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

;; adapted from https://emacs.stackexchange.com/questions/43651/moving-a-subtree-to-the-top-or-bottom-of-its-parent
(defun my-org-move-subtree-to-top ()
  "Move the current subtree to the top of its parent"
  (interactive)
  (condition-case err
      (while t
        (org-move-subtree-up))
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg)))))))
(defun my-org-move-subtree-to-bottom ()
  "Move the current subtree to the bottom of its parent"
  (interactive)
  (condition-case err
      (while t
        (org-move-subtree-down))
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg)))))))
(define-key org-mode-map (kbd "C-c <up>") 'my-org-move-subtree-to-top)
(define-key org-mode-map (kbd "C-c <down>") 'my-org-move-subtree-to-bottom)

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
