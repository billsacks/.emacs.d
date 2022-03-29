(require 'prelude-programming)
(require 'lsp-mode)
(add-hook 'f90-mode-hook #'lsp)

;; Define a function to align all commas on a line
;; adapted from http://www.emacswiki.org/emacs/AlignCommands
(defun f90-align-args (start end)
  "Align all commas on a line"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 0 t))

;; Define a function to align the pieces of a Fortran argument list:
;; align on comma, then '::', then '!'
;; For some reason, this doesn't seem to work consistently (e.g., sometimes fails to work completely on the last line)
(defun f90-align-arg-hdr (start end)
  "Align the pieces of a Fortran argument list"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\),")
  (align-regexp start end
                "\\(\\s-*\\)::")
  (align-regexp start end
                "\\(\\s-*\\)!"))

;; Define a function to align the pieces of a Fortran variable declaration section:
;; align on '::', then '!'
(defun f90-align-var-decl (start end)
  "Align the pieces of a fortran variable declaration section"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)::")
  (align-regexp start end
                "\\(\\s-*\\)!"))

;; Define a function to align assertions
(defun f90-align-asserts (start end)
  "Align equal signs and errMsg portions of assertions"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)==")
  (align-regexp start end
                "\\(\\s-*\\)errMsg"))

;; Define a function to align items in an associate clause
;; (For best results, put a temporary comma before the '&' on the last line, which you can delete after running this function
(defun f90-align-associate (start end)
  "Align pointer and comma in associate clause"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=>")
  (align-regexp start end
                "\\(\\s-*\\),"))

(defun prelude-f90-mode-defaults ()
  "Defaults for Fortran programming"
  (auto-fill-mode 1)
  (flycheck-mode -1)

  ;; from https://emacs.stackexchange.com/questions/20900/navigate-by-indentation/20965#20965
  ;;
  ;; then we can hide the body of blocks with outline-hide-subtree
  ;;
  ;; I may want to change this regex to give a full list of Fortran keywords that can
  ;; start a block, as is done in python mode (see also
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/)
  (setq-local outline-regexp "\\`\\|\\s-+\\S-"))
(setq prelude-f90-mode-hook 'prelude-f90-mode-defaults)
(add-hook 'f90-mode-hook (lambda ()
                           (run-hooks 'prelude-f90-mode-hook)))
