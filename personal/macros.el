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

;; function to copy a line
;; from http://emacs.wordpress.com/2007/01/22/killing-yanking-and-copying-lines/
;; see also http://curiousprogrammer.wordpress.com/2009/02/11/simple-emacs-shortcut/
(defun copy-current-line ()
  "Copy current line into the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2))
  (message "Line copied"))

(defun fit-window-with-max ()
  "Call fit-window-to-buffer with a max width"
  (interactive)
  (fit-window-to-buffer
   ;; the 4th argument is max-width; this is tuned to my LG external monitor
   nil nil nil 180))
