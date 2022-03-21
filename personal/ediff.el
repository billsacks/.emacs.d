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
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map (kbd "C-c d") 'ediff-copy-A-and-B-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun ediff-copy-B-and-A-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer))))
(defun add-upperd-to-ediff-mode-map () (define-key ediff-mode-map (kbd "C-c D") 'ediff-copy-B-and-A-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-upperd-to-ediff-mode-map)

;; similar to the above, but for comparing regions within ediff
(defun my-ediff-compare-regions-no-whitespace ()
  "Invoke ediff-inferior-compare-regions, but ignoring whitespace"
  (interactive)
  (let ((ediff-diff-options (concat ediff-diff-options " -w"))
        (ediff-actual-diff-options (concat ediff-actual-diff-options " -w")))
    (ediff-inferior-compare-regions)))
(defun my-add-equals-to-ediff-mode-map () (define-key ediff-mode-map (kbd "C-c =") 'my-ediff-compare-regions-no-whitespace))
(add-hook 'ediff-keymap-setup-hook 'my-add-equals-to-ediff-mode-map)

(defun my-ediff-buffers-no-whitespace()
  "Invoke ediff-buffers, but ignoring whitespace"
  (interactive)
  (let ((ediff-diff-options (concat ediff-diff-options " -w"))
        (ediff-actual-diff-options (concat ediff-actual-diff-options " -w")))
    (call-interactively 'ediff-buffers)))
