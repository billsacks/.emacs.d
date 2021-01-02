;; For ediff, add options to copy both variants to the final buffer
;; 'c' copies 'a' then 'b'; 'd' copies 'b' then 'a'
;; Credit: http://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version
(defun ediff-copy-A-and-B-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-c-to-ediff-mode-map () (define-key ediff-mode-map "c" 'ediff-copy-A-and-B-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-c-to-ediff-mode-map)

(defun ediff-copy-B-and-A-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-B-and-A-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
