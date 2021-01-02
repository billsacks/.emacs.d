;; Set associations between file types and modes

;; open ChangeLog files in text mode, because the CESM ChangeLog files
;; don't follow emacs' expected ChangeLog format
(setq auto-mode-alist (cons '("ChangeLog" . text-mode) auto-mode-alist))
