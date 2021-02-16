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

;; My original plan was to write a function to make a window 2/3 the total width of the
;; frame, but the following is easier and works well enough for now (on my LG monitor)
(defun enlarge-window-50 ()
  (interactive)
  (enlarge-window-horizontally 50))

;; adapted from https://gist.github.com/danmayer/1009137
(defun copy-buffer-to-other-window ()
  "Copy the buffer from the selected window to the next window"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this)))
    (set-window-buffer other this-buffer)))

(defun move-buffer-to-other-window ()
  "Move the buffer from the selected window in next window"
  (interactive)
  (copy-buffer-to-other-window)
  (bury-buffer)
  (other-window 1) ;;swap cursor to new buffer
  )

;; workaround for scroll bars disappearing when resizing a frame
(defun fix-scroll-bars ()
  (interactive)
  (toggle-scroll-bar -1)
  (toggle-scroll-bar +1))

;; From https://emacs.stackexchange.com/questions/19861/how-to-unhighlight-symbol-highlighted-with-highlight-symbol-at-point
(require 'hi-lock)
(defun unhighlight-all-in-buffer ()
  "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
  (interactive)
  (unhighlight-regexp t))

(defun scroll-up-by-3 ()
  (interactive)
  (scroll-up-command 3))
(put 'scroll-up-by-3 'isearch-scroll t)

(defun scroll-down-by-3 ()
  (interactive)
  (scroll-down-command 3))
(put 'scroll-down-by-3 'isearch-scroll t)

(defun scroll-up-by-20 ()
  (interactive)
  (scroll-up-command 20))
(put 'scroll-up-by-20 'isearch-scroll t)

(defun scroll-down-by-20 ()
  (interactive)
  (scroll-down-command 20))
(put 'scroll-down-by-20 'isearch-scroll t)
