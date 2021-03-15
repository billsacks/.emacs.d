;; For ediff, add options to copy both variants to the final buffer
;; 'd' copies 'a' then 'b'; 'D' copies 'b' then 'a'
;; Credit: http://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version
(require 'ediff)
(defun ediff-copy-A-and-B-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-A-and-B-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun ediff-copy-B-and-A-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer))))
(defun add-upperd-to-ediff-mode-map () (define-key ediff-mode-map "D" 'ediff-copy-B-and-A-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-upperd-to-ediff-mode-map)

;; This can be useful to give simplified diffs when there are whitespace changes I want to
;; ignore. However, I want the default to be showing whitespace changes in diffs. So I
;; will have a separate key binding to explicitly open a magit-ediff session using this
;; function to ignore whitespace differences in diffs. I believe this is set up properly
;; so that subsequent invocations of ediff (without this function) will go back to
;; including whitespace differences, though there may be some edge cases where this
;; doesn't work right.
;;
;; Note that there is also a variable ediff-actual-diff3-options, but I don't currently
;; set that; I can add a setting of that if I find it's needed in some cases.
(defun my-magit-ediff-no-whitespace ()
  "Invoke magit-ediff-dwim, but ignoring whitespace"
  (interactive)
  (let ((ediff-diff-options (concat ediff-diff-options " -w"))
        (ediff-actual-diff-options (concat ediff-actual-diff-options " -w")))
    (magit-ediff-dwim)))
