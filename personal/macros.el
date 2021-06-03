;; general-purpose macros

(defun my-backward-symbol ()
  (interactive)
  (forward-symbol -1))

;; like forward-symbol and backward-symbol, but all the way to next whitespace; however,
;; this doesn't handle newlines the way I would like
(defun my-forward-to-whitespace ()
  (interactive)
  ;; first skip to first non-whitespace; this allows the function to be called repeatedly
  (skip-syntax-forward "-")
  ;; now skip to first whitespace
  (skip-syntax-forward "^-"))
(defun my-backward-to-whitespace ()
  (interactive)
  ;; first skip to first non-whitespace; this allows the function to be called repeatedly
  (skip-syntax-backward "-")
  ;; now skip to first whitespace
  (skip-syntax-backward "^-"))

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

;; I often want to make a new frame but don't want the current buffer appearing there
(defun new-frame-with-scratch-buffer ()
  "Open a new frame with the scratch buffer"
  (interactive)
  (switch-to-buffer-other-frame "*scratch*"))

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

(defun scroll-up-by-10 ()
  (interactive)
  (scroll-up-command 10))
(put 'scroll-up-by-10 'isearch-scroll t)

(defun scroll-down-by-10 ()
  (interactive)
  (scroll-down-command 10))
(put 'scroll-down-by-10 'isearch-scroll t)

(defun my-scroll-other-window-up-by-3 ()
  (interactive)
  (scroll-other-window 3))
(defun my-scroll-other-window-down-by-3 ()
  (interactive)
  (scroll-other-window-down 3))
(defun my-scroll-other-window-up-by-10 ()
  (interactive)
  (scroll-other-window 10))
(defun my-scroll-other-window-down-by-10 ()
  (interactive)
  (scroll-other-window-down 10))

;; This is useful when I have multiple ediff sessions with the same buffer: In this case,
;; the highlighting of the different ediff sessions is shown in all sessions. I often am
;; using multiple frames in this case (one frame for each ediff session). So for now I'm
;; dealing with this by: Instead of simply switching frames, I'm first undoing the
;; highlighting of the current ediff session then switching frames. Then, when I get into
;; an ediff session where I had already undone highlighting like this, I can reapply
;; highlighting by typing 'h' once. (I could probably make it so that, when it gets to the
;; new frame, if the buffer is in ediff mode, then it toggles highlighting once... I'm not
;; sure if I'd want that in general.)
(defun my-ediff-unhighlight-and-switch-frames ()
  (interactive)
  ;; Toggle highlighting 3 times to get from standard highlighting to no highlighting.
  ;; I should probably change this to use a function that does ediff-toggle-hilit in a
  ;; loop, so it could keep doing it until the highlighting value is the desired value;
  ;; then call that function if the mode of the current buffer is ediff mode.
  (ediff-toggle-hilit)
  (ediff-toggle-hilit)
  (ediff-toggle-hilit)
  (call-interactively 'select-frame-by-name))

;; This is useful when a buffer doesn't start which-function-mode by default. This happens
;; with buffers created by magit with a version at the end
(defun my-enable-which-function-mode ()
  (interactive)
  (which-function-mode 1))

;; This is useful so I can cycle frames both forwards and backwards
(defun my-other-frame-reverse ()
  (interactive)
  (other-frame -1))
