(require 'company)

;; Allow starting company completion manually. Now that I am back to using a delay, I want
;; this to be really easy to press, because I envision often triggering completion
;; manually. This key binding with M is also nice because M is used to select a particular
;; completion, and my thumb will already be there.
(global-set-key (kbd "M-SPC") 'company-complete)

;; Sometimes I just want to insert a newline, not have company pick the
;; current selection. Having return bound to company-complete-selection
;; is especially problematic when the top candidate is (currently typed
;; text) + (some suffix); this can particularly happen when candidates
;; are sorted by occurrence (or anything other than alphabetically, I
;; guess). To get around this, use A-M-SPC rather than Ret for
;; company-complete-selection. I have chosen this binding to be similar to the binding I
;; set above for company-complete.
;;
;; (See also https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode)
;;
;; Also note that I am leaving nothing bound to company-complete-common: this is bound to
;; tab by default, but that is asymmetric with my new use of M-SPC bindings for
;; completion; it doesn't feel necessary to have a special binding for
;; company-complete-common since company-complete does that the first time it's run.
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "A-M-SPC") 'company-complete-selection)

  ;; Do not try to auto-complete comments or plain text
  ;; need to customize company-backends this way rather than via standard customization so
  ;; that we avoid overriding backends added by prelude (e.g., anaconda)
  (setq company-backends (remove 'company-dabbrev company-backends)))
