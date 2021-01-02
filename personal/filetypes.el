;; Set associations between file types and modes

;; open ChangeLog files in text mode, because the CESM ChangeLog files
;; don't follow emacs' expected ChangeLog format
(setq auto-mode-alist (cons '("ChangeLog" . text-mode) auto-mode-alist))

;; make .pf files open in f90 mode
(setq auto-mode-alist (cons '("\.pf$" . f90-mode) auto-mode-alist))
