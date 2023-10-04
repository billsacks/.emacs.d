;;; This has changes for both org itself and deft

(require 'org)
(require 'deft)
(require 'org-super-agenda)
(require 'org-id)
(require 'org-latex-impatient)
(require 'real-auto-save)

(org-super-agenda-mode +1)

;; Auto-save org buffers
;; (From the suggested hook in https://github.com/alphapapa/salv.el)
(add-hook 'org-mode-hook
          (lambda ()
            ;; the first condition (buffer-file-name) checks if this is a file-visiting
            ;; buffer, because file-in-directory-p will fail on a non-file-visiting buffer
            (when (and (buffer-file-name) (file-in-directory-p (buffer-file-name) "~/org"))
              (real-auto-save-mode))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+olp "~/org/todo/inbox.org" "-Inbox" "PR-- -inbox misc")
         "* TODO %?\n"
         :prepend t)))

;; Use a separate frame for org-capture
;; From https://www.windley.com/archives/2010/12/capture_mode_and_emacs.shtml
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))
(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))
;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
          'delete-other-windows)
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
                (width . 120)
                (height . 15)))
  (select-frame-by-name "capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (org-capture))

;; From https://emacs.stackexchange.com/questions/3929/make-isearch-skip-folded-content-in-org-mode
(defun my-org-do-not-search-invisible ()
  "Do not search invisible text in this buffer"
  (make-local-variable 'search-invisible)
  (setq search-invisible nil))
(add-hook 'org-mode-hook #'my-org-do-not-search-invisible)

;; in org-todo-keywords, the first line gives a typical progression of states; following
;; lines give less common states
(setq org-todo-keywords '((sequence "TODO(t)" "SOON(s)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
                          (sequence "|" "CANC(c)")
                          ;; PCAN = canceling just because I'm canceling the parent (so if
                          ;; I restore the parent, I'll want to restore this)
                          (sequence "|" "PCAN(C)")
                          ;; MBCN = maybe canceled
                          (sequence "MBCN(b)" "MAYB(m)" "COND(o)" "HOLD(h)" "WAIT(w)" "DPND(e)" "|")
                          ;; the following are for projects, both in my top-level projects
                          ;; file and in files for individual projects that are big enough
                          ;; that they are broken down into relatively large subprojects:
                          ;; - PR-1 is for ones I'm working on now (this week)
                          ;; - PR-2 is for relatively high priority, but not quite yet
                          ;; - PR-3 is normal priority
                          ;; - PR-4 is sometime in the more distant future
                          ;; - PR-0 is for projects that I'm helping with, so I'm not in
                          ;;   control of the timeline or priority, but need to just
                          ;;   respond whenever I'm needed (these can shift to some other
                          ;;   priority when it comes time for me to deal with them)
                          ;; - PR-D is for done projects (so that I can separately search
                          ;;   for done projects, separate from done tasks) (I'm using a
                          ;;   key of '.' because a period signifies the end of something,
                          ;;   and this keeps with a rule that PR states are marked with
                          ;;   numbers and symbols)
                          ;; - PR-C is for canceled projects (with a key of ',', similar
                          ;;   to the '.' for PR-D)
                          (sequence "PR-0(0)" "PR-4(4)" "PR-3(3)" "PR-2(2)" "PR-1(1)" "|" "PR-D(.)")
                          (sequence "|" "PR-C(,)")
                          ;; this is kind of like a project, but is to just collect misc
                          ;; tasks; it is never expected that this will be marked as done
                          (sequence "PR--(-)" "|")))

(setq org-todo-keyword-faces
      '(("TODO" . "SteelBlue3")
        ("SOON" . "blue")
        ("NEXT" . "red")
        ("PROG" . (:inherit org-todo :foreground "red" :slant italic))
        ("MBCN" . (:inherit org-todo :foreground "LightSteelBlue3" :strike-through t))
        ("MAYB" . "LightSteelBlue3") ("COND" . "LightSteelBlue3")
        ("HOLD" . "purple") ("WAIT" . "purple") ("DPND" . "purple")
        ("DONE" . (:inherit org-done :foreground "gray40"))
        ("CANC" . (:inherit org-done :foreground "gray40" :strike-through t))
        ("PCAN" . (:inherit org-done :foreground "gray40" :strike-through t))
        ;; For the PR-# states: I want a distinct color for PR-1, since that is the stuff
        ;; I should pay attention to this week. For the other three, I am using DarkBlue
        ;; for 2, LightBlue3 for 4, and an intermediate color (from
        ;; https://meyerweb.com/eric/tools/color-blend) for 3.
        ("PR-1" . (:inherit org-todo :foreground "DarkRed" :background "#d1eaff"))
        ("PR-2" . (:inherit org-todo :foreground "DarkBlue" :background "#d1eaff"))
        ("PR-3" . (:inherit org-todo :foreground "#4D60AC" :background "#d1eaff"))
        ("PR-4" . (:inherit org-todo :foreground "LightBlue3" :background "#d1eaff"))
        ("PR-0" . (:inherit org-todo :foreground "gray25" :background "#d1eaff"))
        ("PR--" . (:inherit org-todo :foreground "gray25" :background "#d1eaff"))
        ("PR-D" . (:inherit org-done :foreground "gray40" :background "#d1eaff"))
        ("PR-C" . (:inherit org-done :foreground "gray40" :strike-through t :background "#d1eaff"))))

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

        (" " "Areas (Level 1 headings)"
         ((tags "+LEVEL=1" ((org-agenda-sorting-strategy '(alpha-up)))))
         nil)

        ("1" "PR-1"
         ((agenda "" nil)
          (todo "PR-1" nil))
         nil)
        ("2" "PR-2"
         ((agenda "" nil)
          (todo "PR-2" nil))
         nil)
        ;; Note that '@' is shift-2; I'm using that key because this is like 2, but a
        ;; variant where it includes any projects with priority equal to or greater than 2
        ("@" "PR-1/2"
         ((agenda "" nil)
          (todo "PR-1|PR-2" nil))
         nil)
        ("3" "PR-3"
         ((agenda "" nil)
          (todo "PR-3" nil))
         nil)
        ("4" "PR-4"
         ((agenda "" nil)
          (todo "PR-4" nil))
         nil)
        ("0" "PR-0"
         ((agenda "" nil)
          (todo "PR-0" nil))
         nil)
        ("-" "PR--"
         ((agenda "" nil)
          (todo "PR--" nil))
         nil)
        ("p" "Projects"
         ((todo "PR-1|PR-2|PR-3|PR-4|PR-0|PR--" nil))
         nil)

        ("x" "SOON"
         ((agenda "" nil)
          (todo "SOON|NEXT|PROG" nil))
         nil)
        ("X" "SOON tree" tags-tree "TODO=\"SOON\"|TODO=\"NEXT\"|TODO=\"PROG\"" nil)

        ("y" "NEXT"
         ((agenda "" nil)
          (todo "NEXT|PROG" nil))
         nil)
        ("Y" "NEXT tree" tags-tree "TODO=\"NEXT\"|TODO=\"PROG\"" nil)

        ("z" "PROG"
         ((agenda "" nil)
          (todo "PROG" nil))
         nil)
        ("Z" "PROG tree" tags-tree "TODO=\"PROG\"" nil)

        ;; this combines PR-1 with the SOON search; mnemonic: "w" = "week"
        ;;
        ;; I am also including the inbox here because I often want to deal with things in
        ;; the inbox when viewing this week's todos
        ("w" "PR-1 & SOON"
         ((agenda "" nil)
          ;; To show things in the inbox, I have added an "inbox" tag to the top-level
          ;; heading (there may be some other way to accomplish this, but adding this tag
          ;; seemed easiest). This tags-todo search will include any projects added to the
          ;; inbox in addition to any tasks under my "inbox misc" PR-- heading. For some
          ;; reason that I can't figure out, it doesn't list the "inbox misc" PR-- heading
          ;; itself, but that turns out to be the behavior I want anyway.
          (tags-todo "inbox" nil)
          (todo "SOON|NEXT|PROG" nil)
          (todo "PR-1" nil))
         nil)
        ))

(defun my-osa-outline-path-length (outline-path)
  ;; Return the number of elements of the outline path to display for org-super-agenda ("osa")
  ;;
  ;; If the second element of the path is "Archive" then we display an extra element in
  ;; order to display the project within the Archive.
  (if (equal (nth 1 outline-path) "Archive")
      3
    2)
  )

;; the following is modified from the built-in outline-path: the first line is the same,
;; but I have replaced the s-join expression with one that only uses the first 2 levels,
;; and am using a different separator for the 2 elements of the path (" | ")
(org-super-agenda--def-auto-group outline-path-2 "their outline paths, first 2 levels"
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (let ((outline-path (org-get-outline-path)))
                (s-join " | " (butlast outline-path (- (length outline-path) (my-osa-outline-path-length outline-path)))))
              ))

(setq org-super-agenda-groups
      '((:auto-outline-path-2 t)))

(global-set-key [remap org-set-tags-command] #'counsel-org-tag)
(define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-decrement-word)

;; compensate for my muscle memory of using backticks for inline code (from
;; https://lists.gnu.org/archive/html/emacs-orgmode/2021-03/msg00499.html) (if I want a
;; backtick, I can get it with C-q)
;;
;; see also
;; https://archive.casouri.cat/note/2020/better-looking-verbatim-markup-in-org-mode/index.html
;; for if I wanted this to actually appear as a backtick
(define-key org-mode-map (kbd "`") (kbd "~"))

(define-key org-mode-map (kbd "C-c k") 'org-cut-special)
(define-key org-mode-map (kbd "C-c w") 'org-copy-special)
(define-key org-mode-map (kbd "C-c y") 'org-paste-special)

(defun my-org-archive-to-archive-sibling-if-okay ()
  "Archive the current heading to an archive sibling if we deem it okay to do so"
  (interactive)
  (if (or (string-equal (org-get-todo-state) "DONE")
          (string-equal (org-get-todo-state) "CANC")
          (string-equal (org-get-todo-state) "PCAN")
          (string-equal (org-get-todo-state) "PR-D")
          (string-equal (org-get-todo-state) "PR-C"))
      (org-archive-to-archive-sibling)
    (message "Refusing to archive heading that isn't in a done / canceled state")))

(define-key org-mode-map (kbd "C-c a") 'my-org-archive-to-archive-sibling-if-okay)
(define-key org-mode-map (kbd "C-c A") 'org-toggle-archive-tag)

;; org-display-outline-text doesn't show the last level; fix that with this function
(defun my-org-show-position-in-text () ;; display outline path of hierarchical headings
  (interactive)
  ;; first argument nil says do not prepend file name; second argument t says do append
  ;; current heading
  (org-display-outline-path nil t))
;; mnemonic: o for "outline" (this is similar to org-get-outline-path)
(define-key org-mode-map (kbd "C-c o") 'my-org-show-position-in-text)

;; Functions to highlight the inline code under cursor
;; Consider replacing with a more general solution: see https://emacspeak.blogspot.com/2021/09/generalize-snarf-tool-how-general-can.html
(defun my-org-highlight-delineated-text (markup_char)
  "Highlight block of text containing point, delineated by markup_char"
  (search-backward markup_char)
  (forward-char 1)
  (set-mark (point))
  (search-forward markup_char)
  (backward-char 1))
(defun my-org-highlight-verbatim ()
  (interactive)
  (my-org-highlight-delineated-text "="))
;; Use C-c i similarly to how I use various I keybindings for highlighting (these are
;; bound to H by default, but I use H for scrolling, so I have rebound H bindings to I
;; instead)
(define-key org-mode-map (kbd "C-c I") 'my-org-highlight-verbatim)
(defun my-org-highlight-inline-code ()
  (interactive)
  (my-org-highlight-delineated-text "~"))
(define-key org-mode-map (kbd "C-c i") 'my-org-highlight-inline-code)

;; like org-forward-heading-same-level but returns t if it moved, nil if not
(defun my-org-forward-heading-same-level ()
  (let ((pos (point)))
    (org-forward-heading-same-level 1)
    (if (eq pos (point))
        nil
      t)))
;; like org-backward-heading-same-level but returns t if it moved, nil if not
(defun my-org-backward-heading-same-level ()
  (let ((pos (point)))
    (org-backward-heading-same-level 1)
    (if (eq pos (point))
        nil
      t)))

;; allow navigation to next / previous todos
(defun my-org-wrap-move-to-todo (cmd)
  "Execute CMD, but make sure that the cursor always ends up in a todo headline.
If not, return to the original position."
  (interactive)
  (let ((pos (point)))
    (funcall cmd)
    (unless (org-entry-is-todo-p)
      (goto-char pos))))
(defun my-org-next-visible-todo-helper ()
  (org-next-visible-heading 1)
  (while (and (not (org-entry-is-todo-p)) (not (eobp)))
    (org-next-visible-heading 1)))
(defun my-org-next-visible-todo ()
  (interactive)
  (my-org-wrap-move-to-todo 'my-org-next-visible-todo-helper))
(defun my-org-previous-visible-todo-helper ()
  (org-previous-visible-heading 1)
  (while (and (not (org-entry-is-todo-p)) (not (bobp)))
    (org-previous-visible-heading 1)))
(defun my-org-previous-visible-todo ()
  (interactive)
  (my-org-wrap-move-to-todo 'my-org-previous-visible-todo-helper))
(defun my-org-forward-todo-same-level-helper ()
  (let ((moved (my-org-forward-heading-same-level)))
    (while (and (not (org-entry-is-todo-p)) (not (eobp)) moved)
      (setq moved (my-org-forward-heading-same-level)))))
(defun my-org-forward-todo-same-level ()
  (interactive)
  (my-org-wrap-move-to-todo 'my-org-forward-todo-same-level-helper))
(defun my-org-backward-todo-same-level-helper ()
  (let ((moved (my-org-backward-heading-same-level)))
    (while (and (not (org-entry-is-todo-p)) (not (bobp)) moved)
      (setq moved (my-org-backward-heading-same-level)))))
(defun my-org-backward-todo-same-level ()
  (interactive)
  (my-org-wrap-move-to-todo 'my-org-backward-todo-same-level-helper))
(define-key org-mode-map (kbd "C-c C-S-n") 'my-org-next-visible-todo)
(define-key org-mode-map (kbd "C-c C-S-p") 'my-org-previous-visible-todo)
(define-key org-mode-map (kbd "C-c C-S-f") 'my-org-forward-todo-same-level)
(define-key org-mode-map (kbd "C-c C-S-b") 'my-org-backward-todo-same-level)

(defun my-org-goto-last-heading-in-subtree ()
  (let ((moved (my-org-forward-heading-same-level)))
    (while moved
      (setq moved (my-org-forward-heading-same-level)))))

;; Use modified arrow keys for some common navigation
(define-key org-mode-map (kbd "C-M-<up>") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "C-M-<down>") 'org-next-visible-heading)

(defun my-org-maybe-set-todo-state ()
  "Possibly change the todo state of the current heading"
  (if (equal (org-current-level) 2)
      (org-todo "PR-1")))

(defun my-org-insert-todo-heading-helper (arg &optional force-heading)
  (if (equal arg '(4))
      (org-insert-todo-heading '(16) force-heading)
    (org-insert-todo-heading '(4) force-heading)))
(defun my-org-insert-todo-heading (arg &optional force-heading)
  "Like org-insert-todo-heading but use the first state.

With no prefix args, this operates like org-insert-todo-heading
with one prefix arg.

With one prefix arg, force inserting at the end of the parent subtree
(like org-insert-todo-heading with two prefix args).

Note: the force-heading piece of this is untested."
  (interactive "P")
  (my-org-insert-todo-heading-helper arg force-heading)
  (my-org-maybe-set-todo-state))
;; (2022-07-06) Disabling this key binding for now because it overrides the standard
;; M-S-return binding. That was intentional but I am finding that this is particularly
;; problematic when inserting a new checklist item ('- [ ]'), in which case, if it is
;; under a level-2 heading in my notes, that heading gets the PR-1 state.
;; (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)

(defun my-org-insert-todo-heading-respect-content-helper ()
  (org-insert-todo-heading-respect-content '(4)))
(defun my-org-insert-todo-heading-respect-content ()
  "Like org-insert-todo-heading-respect-content but use the first state"
  (interactive)
  (my-org-insert-todo-heading-respect-content-helper)
  (my-org-maybe-set-todo-state))
(define-key org-mode-map (kbd "<C-S-return>") 'my-org-insert-todo-heading-respect-content)
;; and here is a more ergonomic way to do this:
;; first we need to redefine C-c C-spc to do what C-c spc does by default:
(define-key org-mode-map (kbd "C-c C-SPC") 'org-table-blank-field)
(define-key org-mode-map (kbd "C-c SPC") 'my-org-insert-todo-heading-respect-content)

(defun my-org-insert-todo-heading-at-end-of-subtree ()
  "Insert a todo heading at the end of the current subtree, but above a possible 'Archive' heading"
  (my-org-goto-last-heading-in-subtree)
  (if (string-equal (org-get-heading t) "Archive")
      ;; Put the todo before the Archive
      (my-org-insert-todo-heading-helper 0)
    ;; Put the todo after this last headline
    (my-org-insert-todo-heading-respect-content-helper)))

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
;; I'm going to use C-c C for my-org-parent-canceled-and-forward instead, since that's more useful
;; (define-key org-mode-map (kbd "C-c C") 'my-org-canceled-and-archive)
(defun my-org-parent-canceled-and-forward ()
  "Change current heading's state to PCAN then go forward to the next todo at the same level"
  (interactive)
  (org-todo "PCAN")
  (my-org-forward-todo-same-level))
(define-key org-mode-map (kbd "C-c C") 'my-org-parent-canceled-and-forward)

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

;; With my current org file organization, my org-based notes are NOT in my list of
;; org-agenda files. By default, org-id-update-id-locations only looks in agenda files and
;; related files. So, in order for org-id-update-id-locations to fix links to my notes
;; files if I rename or move these files, we need to tell org-id about all of them.
;;
;; I am making this an interactive function so that I can rerun it to update
;; org-id-extra-files without restarting my emacs session.
(defun my-org-set-org-id-extra-files ()
  "Update org-id-extra-files to contain all of my org files"
  (interactive)
  ;; Note that the out-of-the-box value is org-agenda-text-search-extra-files
  (setq org-id-extra-files (directory-files-recursively "~/org" "\.org$")))
;; And run this function when starting emacs
(my-org-set-org-id-extra-files)

(defun my-org-store-link-to-file ()
  "Store a link to the current org file - not a subtree"
  (interactive)
  ;; set org-id-link-to-org-use-id to nil for this command to avoid creating an unnecessary id property
  (let ((org-id-link-to-org-use-id nil))
    ;; in the following, the arg of '(4) prevents storing the context, so just stores a
    ;; link to the file; the arg of t acts as if this is called interactively, which seems
    ;; important for some reason
    (org-store-link '(4) t)))

(defun my-org-tree-to-indirect-buffer ()
  "My version of org-tree-to-indirect-buffer"
  (interactive)
  ;; Call org-tree-to-indirect-buffer with a prefix argument: this causes it to create a
  ;; new buffer rather than reusing an existing buffer.
  (org-tree-to-indirect-buffer '(4)))
(define-key org-mode-map (kbd "C-c b") 'my-org-tree-to-indirect-buffer)

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

(defun my-org-insert-checkbox ()
  "Insert a checkbox at point"
  (interactive)
  (insert "- [ ] "))
(define-key org-mode-map (kbd "C-c [") 'my-org-insert-checkbox)

(defun my-deft-mode-hook ()
  (hl-line-mode +1))
(add-hook 'deft-mode-hook #'my-deft-mode-hook)

;; Allow exporting as Github-flavored markdown
(eval-after-load "org"
  '(require 'ox-gfm nil t))
;; Allow exporting as rst
(eval-after-load "org"
  '(require 'ox-rst nil t))

;; This is available as C-c C-t, but I often do C-c t by accident, and since this is a
;; common thing I want to do, I'll make it a key binding
(define-key org-mode-map (kbd "C-c t") 'org-todo)

;; Hide / show body text. If there is a lot of body text before subheadings (children) it
;; can be helpful sometimes to hide the body text of an entry. Conversely, body text often
;; starts out hidden, so it can be helpful to show it.
(define-key org-mode-map (kbd "C-c e") 'org-show-entry)
(define-key org-mode-map (kbd "C-c E") 'org-hide-entry)

(defun my-org-open-projects ()
  "Open my _projects.org file"
  (interactive)
  (find-file "~/org/_projects.org"))

(defun my-org-open-inbox ()
  "Open the inbox with point positioned at start"
  (interactive)
  (find-file-other-window "~/org/todo/inbox.org")
  (goto-char (point-min))
  (org-next-visible-heading 1))

;; This is useful for jotting down some quick notes / tasks
(defun my-org-open-scratch ()
  "Open scratch buffer in other window, with cursor positioned at end"
  (interactive)
  (switch-to-buffer-other-window "*scratch*")
  (goto-char (point-max)))

(defun my-org-last-buffer-in-other-window ()
  "Open the last org buffer in the other window on this frame"
  (interactive)
  (other-window 1)
  ;; For some reason the following works without even needing to send kbd Return to select
  ;; the first option:
  (execute-kbd-macro (kbd "M-x org-switchb")))

;; my-org-show-agenda isn't very useful now that I have org-agenda-sticky set to t
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

;; Copy region from org as markdown
;; From http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard
(defun my-org-copy-region-as-markdown ()
  "Copy the region (in Org) to the system clipboard as Markdown."
  (interactive)
  (if (use-region-p)
      (let* ((region
	      (buffer-substring-no-properties
	       (region-beginning)
	       (region-end)))
	     (markdown
	      (org-export-string-as region 'md t '(:with-toc nil))))
	(gui-set-selection 'CLIPBOARD markdown))))

;; Facilitate pasting screen shots into org documents
;; From https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun my-org-insert-clipboard-image ()
  (interactive)
  (let ((filename (concat
                   (make-temp-name
                    (concat (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))
                            "_imgs/"
                            (format-time-string "%Y%m%d_%H%M%S_")) ) ".png")))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
    (shell-command (concat "pngpaste " filename))
    (insert (concat "[[file:" filename "]]"))
    (org-display-inline-images)
    )
  )

;; Enable org-latex-impatient-mode
;;
;; But I only want it to be triggered manually, not automatically. This is partly to avoid
;; a theoretical performance hit (that may or may not be an actual problem, and probably
;; isn't) with periodically checking whether we're in a latex region; and moreover to
;; prevent it from trying to render before we're ready for it.
(defun org-latex-impatient--prepare-timer (&rest _)
  ;; do nothing
  )
(define-key org-mode-map (kbd "C-c l") 'org-latex-impatient-start)
(define-key org-mode-map (kbd "C-c L") 'org-latex-impatient-stop)
;; (2022-04-18) Actually, not enabling this for now: I'm not sure whether I'm going to use
;; org-latex-impatient-mode in org-mode, or generally just edit latex in a separate latex
;; buffer (with C-c '), in which case I would turn on this org-latex-impatient-mode (or
;; something similar) there. But I'm keeping the above in place, so that if/when I do
;; enable org-latex-impatient, it is set up to just work manually, rather than being
;; triggered based on a timer.
;;
;; (add-hook 'org-mode-hook #'org-latex-impatient-mode)

;; In source code blocks, I still want to be able to use tab to indent... it seems that,
;; with my org settings (particularly, I think, setting org-cycle-emulate-tab to nil) tab
;; will always cycle the heading, so I need to provide an alternative way to do it. Note
;; that this doesn't work totally correctly, but it's better than nothing.
;;
;; An alternative might be: in org mode, map tab to a function that checks if we're inside
;; a source block (is there an org function for that?); if so, calls
;; indent-for-tab-command; if not, does whatever tab normally does (org-cycle?).
;;
;; Another possible solution may be polymode / poly-org (which says it prevents the need
;; for popping open a separate buffer for editing source blocks with C-c ').
(define-key org-mode-map (kbd "C-<tab>") 'indent-for-tab-command)

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

;; s-o keybindings: Org-related commands that can be run from anywhere (not just an org-mode buffer)
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-o a") 'org-agenda)
(global-set-key (kbd "s-o b") 'org-switchb)
(global-set-key (kbd "s-o B") 'my-org-last-buffer-in-other-window)
(global-set-key (kbd "s-o SPC") 'org-capture)
(global-set-key (kbd "s-o d a") 'my-deft-in-archive)
(global-set-key (kbd "s-o d n") 'my-deft-in-notes)
(global-set-key (kbd "s-o d s") 'my-deft-in-someday)
(global-set-key (kbd "s-o d t") 'my-deft-in-todo)
(global-set-key (kbd "s-o i") 'my-org-open-inbox)
(global-set-key (kbd "s-o l f") 'my-org-store-link-to-file)
(global-set-key (kbd "s-o l l") 'org-store-link)
(global-set-key (kbd "s-o p") 'my-org-open-projects)
(global-set-key (kbd "s-o s") 'my-org-open-scratch)
