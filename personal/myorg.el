;;; This has changes for both org itself and deft

(require 'org)
(require 'deft)
(require 'org-super-agenda)

(org-super-agenda-mode +1)

;; in org-todo-keywords, the first line gives a typical progression of states; following
;; lines give less common states
(setq org-todo-keywords '((sequence "TODO(t)" "SOON(s)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
                          (sequence "|" "CANC(c)")
                          ;; MBCN = maybe canceled
                          (sequence "MBCN(b)" "MAYB(m)" "COND(o)" "HOLD(h)" "WAIT(w)" "DPND(e)" "|")
                          ;; the following are for projects, both in my top-level projects
                          ;; file and in files for individual projects that are big enough
                          ;; that they are broken down into relatively large subprojects:
                          ;; - PR-1 is for ones I'm working on now (this week)
                          ;; - PR-2 is for relatively high priority, but not quite yet
                          ;; - PR-3 is normal priority
                          ;; - PR-4 is sometime in the more distant future
                          (sequence "PR-4(4)" "PR-3(3)" "PR-2(2)" "PR-1(1)" "|")))

(setq org-todo-keyword-faces
      '(("TODO" . "SteelBlue3")
        ("SOON" . "blue")
        ("NEXT" . "red")
        ("PROG" . (:inherit org-todo :foreground "red" :slant italic))
        ("MBCN" . (:inherit org-todo :foreground "LightSteelBlue3" :strike-through t))
        ("MAYB" . "LightSteelBlue3") ("COND" . "LightSteelBlue3")
        ("HOLD" . "purple") ("WAIT" . "purple") ("DPND" . "purple")
        ("DONE" . (:inherit org-done :foreground "RosyBrown"))
        ("CANC" . (:inherit org-done :foreground "RosyBrown" :strike-through t))
        ;; For the PR-# states: I want a distinct color for PR-1, since that is the stuff
        ;; I should pay attention to this week. For the other three, I am using DarkBlue
        ;; for 2, LightBlue3 for 4, and an intermediate color (from
        ;; https://meyerweb.com/eric/tools/color-blend) for 3.
        ("PR-1" . (:inherit org-todo :foreground "DarkRed" :background "#d1eaff"))
        ("PR-2" . (:inherit org-todo :foreground "DarkBlue" :background "#d1eaff"))
        ("PR-3" . (:inherit org-todo :foreground "#4D60AC" :background "#d1eaff"))
        ("PR-4" . (:inherit org-todo :foreground "LightBlue3" :background "#d1eaff"))))

;; It seems that customizing the face for an org-tag like this loses the feature that a
;; tag is colored according to its heading level - so these tags end up with the color of
;; the default face. Oh well: in some ways that actually seems good.
(setq org-tag-faces
      '(
        ;; I use the tag "large" in _projects.org and at the top of org files for large
        ;; projects to denote that this is a large project, with subprojects; I want this
        ;; tag to stand out.
        ("large" . (:inherit org-tag :underline t))
        ))

(setq org-tags-exclude-from-inheritance '("large"))

;; I initially set this via the customization interface, then moved it from custom.el to
;; here. The "n" entry was present initially, so I'm leaving it as is; the others are my
;; additions. I couldn't get todo-tree to work with multiple states, so I'm using
;; tags-tree instead.
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda "" nil)
          (alltodo "" nil))
         nil)

        ("x" "SOON"
         ((todo "SOON|NEXT|PROG" nil)
          (agenda "" nil))
         nil)
        ("X" "SOON tree" tags-tree "TODO=\"SOON\"|TODO=\"NEXT\"|TODO=\"PROG\"" nil)

        ("y" "NEXT"
         ((todo "NEXT|PROG" nil)
          (agenda "" nil))
         nil)
        ("Y" "NEXT tree" tags-tree "TODO=\"NEXT\"|TODO=\"PROG\"" nil)

        ("z" "PROG"
         ((todo "PROG" nil)
          (agenda "" nil))
         nil)
        ("Z" "PROG tree" tags-tree "TODO=\"PROG\"" nil)))

;; the following is modified from the built-in outline-path: the first line is the same,
;; but I have replaced the s-join expression with one that only uses the first 2 levels,
;; and am using a different separator for the 2 elements of the path (" | ")
(org-super-agenda--def-auto-group outline-path-2 "their outline paths, first 2 levels"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (let ((outline-path (org-get-outline-path)))
                (s-join " | " (butlast outline-path (- (length outline-path) 2))))
              ))

(setq org-super-agenda-groups
      '((:auto-outline-path-2 t)))

(global-set-key [remap org-set-tags-command] #'counsel-org-tag)
(define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-decrement-word)

(define-key org-mode-map (kbd "C-c k") 'org-cut-special)
(define-key org-mode-map (kbd "C-c w") 'org-copy-special)
(define-key org-mode-map (kbd "C-c y") 'org-paste-special)

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

;; allow navigation to next / previous todos
(defun my-org-next-visible-todo ()
  (interactive)
  (org-next-visible-heading 1)
  (while (and (not (org-entry-is-todo-p)) (not (eobp)))
    (org-next-visible-heading 1)))
(defun my-org-previous-visible-todo ()
  (interactive)
  (org-previous-visible-heading 1)
  (while (and (not (org-entry-is-todo-p)) (not (bobp)))
    (org-previous-visible-heading 1)))
(defun my-org-forward-todo-same-level ()
  (interactive)
  (org-forward-heading-same-level 1)
  (while (and (not (org-entry-is-todo-p)) (not (eobp)))
    (org-forward-heading-same-level 1)))
(defun my-org-backward-todo-same-level ()
  (interactive)
  (org-backward-heading-same-level 1)
  (while (and (not (org-entry-is-todo-p)) (not (bobp)))
    (org-backward-heading-same-level 1)))
(define-key org-mode-map (kbd "C-c C-S-n") 'my-org-next-visible-todo)
(define-key org-mode-map (kbd "C-c C-S-p") 'my-org-previous-visible-todo)
(define-key org-mode-map (kbd "C-c C-S-f") 'my-org-forward-todo-same-level)
(define-key org-mode-map (kbd "C-c C-S-b") 'my-org-backward-todo-same-level)

;; Allow modified arrow keys for navigation between headings, too
(define-key org-mode-map (kbd "C-M-<up>") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "C-M-<down>") 'org-next-visible-heading)
(define-key org-mode-map (kbd "C-M-<left>") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "C-M-<right>") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "C-M-S-<up>") 'my-org-previous-visible-todo)
(define-key org-mode-map (kbd "C-M-S-<down>") 'my-org-next-visible-todo)
(define-key org-mode-map (kbd "C-M-S-<left>") 'my-org-backward-todo-same-level)
(define-key org-mode-map (kbd "C-M-S-<right>") 'my-org-forward-todo-same-level)
;; C-A-up feels a bit more intuitive for outline-up-heading, but C-A-left is a bit easier
;; on my hands
(define-key org-mode-map (kbd "C-A-<left>") 'outline-up-heading)

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
(define-key org-mode-map (kbd "<M-A-S-return>") 'my-org-insert-todo-heading)

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

(defun my-org-goto-project-planner ()
  "Go to the entry in the project planning doc corresponding to the current file"
  (interactive)
  (let ((mybuffer "my-org-goto-project-planner-buffer"))
    (org-ql-search "~/org/_projects.org"
      `(link :target ,(concat "todo/" (file-name-nondirectory (buffer-file-name))))
      :buffer mybuffer
      )

    ;; The following assumes that there is only one result; if we wanted this to be more
    ;; robust, we could first determine the number of results somehow (maybe by running
    ;; the query twice, one time specifying an action that stores results in a list, if
    ;; there is no better way???), then: if there is just one result, switch to it,
    ;; otherwise keep the buffer open so I can decide what to do.
    ;;
    ;; how to do this:
    ;; - (count-lines (point-min) (point-max)) -- make sure this is 1, otherwise abort
    ;; - check if current line is empty, using (looking-at-p "[[:space:]]*$") [from https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace]; if so, abort
    (switch-to-buffer mybuffer)
    (org-agenda-switch-to)
    (kill-buffer mybuffer)))
(define-key org-mode-map (kbd "C-c p p") 'my-org-goto-project-planner)

(defun my-org-archive-project ()
  "Archive the current project: both the project's org file and its reference in _projects.org"
  (interactive)
  (my-org-move-to-archive)

  (let ((project-buffer (current-buffer)))
    ;; remove this line from _projects.org
    (my-org-goto-project-planner)
    (org-archive-subtree)
    (save-buffer)

    ;; we had to wait a bit to kill the project buffer - at least until after calling
    ;; my-org-goto-project-planner; but now we're done with it so we can kill it
    (kill-buffer project-buffer)))
(define-key org-mode-map (kbd "C-c p a") 'my-org-archive-project)

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
;; can be helpful sometimes to hide the body text of an entry. Conversely, body text often
;; starts out hidden, so it can be helpful to show it.
(define-key org-mode-map (kbd "C-c b") 'org-show-entry)
(define-key org-mode-map (kbd "C-c B") 'org-hide-entry)

(defun my-org-open-projects ()
  "Open my _projects.org file"
  (interactive)
  (find-file "~/org/_projects.org"))
(defun my-org-show-agenda ()
  "Show the existing *Org Agenda* buffer"
  (interactive)
  (switch-to-buffer "*Org Agenda*"))

;; Functions to help with moving files between my org directories
(defun my-org-move-to-archive ()
  "Move the current buffer's file to the archive directory"
  (interactive)
  (my-move-buffer-file "~/org/todo/archive"))
(defun my-org-move-to-notes ()
  "Move the current buffer's file to the notes directory"
  (interactive)
  (my-move-buffer-file "~/org/notes"))
(defun my-org-move-to-someday ()
  "Move the current buffer's file to the someday directory"
  (interactive)
  (my-move-buffer-file "~/org/todo/someday"))
(defun my-org-move-to-todo ()
  "Move the current buffer's file to the todo directory"
  (interactive)
  (my-move-buffer-file "~/org/todo"))
(define-key org-mode-map (kbd "C-c m a") 'my-org-move-to-archive)
(define-key org-mode-map (kbd "C-c m n") 'my-org-move-to-notes)
(define-key org-mode-map (kbd "C-c m s") 'my-org-move-to-someday)
(define-key org-mode-map (kbd "C-c m t") 'my-org-move-to-todo)

;; Allow using Deft for all of these different directories
;;
;; Based on https://www.emacswiki.org/emacs/DeftMode and
;; http://pragmaticemacs.com/emacs/deft-as-a-file-search-tool/
(defun my-deft-in-dir (dir)
  "Run deft in directory DIR"
  (setq deft-directory dir)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (deft))
(defun my-deft-in-archive ()
  "Run deft in the archive directory"
  (interactive)
  (my-deft-in-dir "~/org/todo/archive"))
(defun my-deft-in-notes ()
  "Run deft in the notes directory"
  (interactive)
  (my-deft-in-dir "~/org/notes"))
(defun my-deft-in-someday ()
  "Run deft in the someday directory"
  (interactive)
  (my-deft-in-dir "~/org/todo/someday"))
(defun my-deft-in-todo ()
  "Run deft in the todo directory"
  (interactive)
  (my-deft-in-dir "~/org/todo"))

;; s-g keybindings: Org-related commands that can be run from anywhere (not just an org-mode buffer)
(global-unset-key (kbd "s-g"))
(global-set-key (kbd "s-g a") 'my-org-show-agenda)
(global-set-key (kbd "s-g A") 'org-agenda)
(global-set-key (kbd "s-g b") 'org-switchb)
(global-set-key (kbd "s-g d a") 'my-deft-in-archive)
(global-set-key (kbd "s-g d n") 'my-deft-in-notes)
(global-set-key (kbd "s-g d s") 'my-deft-in-someday)
(global-set-key (kbd "s-g d t") 'my-deft-in-todo)
(global-set-key (kbd "s-g l") 'org-store-link)
(global-set-key (kbd "s-g p") 'my-org-open-projects)

;; (2021-08-10) I commonly need to type a tilde in org mode; it's a bit difficult, and my
;; left pinky has been hurting recently, so I'm adding this alternative (mnemonic: c for
;; code).
(defun my-org-insert-tilde ()
  "Insert a tilde"
  (interactive)
  (insert "~"))
(define-key org-mode-map (kbd "A-c") 'my-org-insert-tilde)
