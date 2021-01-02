;; Sometimes I just want to insert a newline, not have company pick the
;; current selection. Having return bound to company-complete-selection
;; is especially problematic when the top candidate is (currently typed
;; text) + (some suffix); this can particularly happen when candidates
;; are sorted by occurrence (or anything other than alphabetically, I
;; guess). To get around this, use S-TAB rather than Ret for
;; company-complete-selection. I also like this key binding because it
;; is similar to TAB (which is used by default for
;; company-complete-common).
;;
;; (Note, though, that in text terminals, the shifted things may not
;; work (see
;; http://ergoemacs.org/emacs_manual/elisp/Other-Char-Bits.html).)
;;
;; (See also https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "S-TAB") 'company-complete-selection)
  (define-key company-active-map [(shift tab)] 'company-complete-selection)
  (define-key company-active-map [backtab] 'company-complete-selection))
