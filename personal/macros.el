;; general-purpose macros

;; swap two strings (from http://stackoverflow.com/questions/768243/interactive-emacs-lisp-function-to-swap-two-words-with-each-other)
(defun swap-words (a b)
  "Replace all occurrences of a with b and vice versa"
  (interactive "*sFirst Swap Word: \nsSecond Swap Word: ")
  (save-excursion
    (while (re-search-forward (concat (regexp-quote a) "\\|" (regexp-quote b)))
      (if (y-or-n-p "Swap?")
          (if (equal (match-string 0) a)
              (replace-match (regexp-quote b))
            (replace-match (regexp-quote a))))
      )))

;; Unfill region (from https://www.emacswiki.org/emacs/UnfillRegion)
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
