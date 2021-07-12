(require 'company)

;; Allow starting company completion manually. Now that I am back to using a delay, I want
;; this to be really easy to press, because I envision often triggering completion
;; manually. This key binding with M is also nice because M is used to select a particular
;; completion, and my thumb will already be there.
;;
;; (2021-03-08) Now that I am no longer using a delay after all, I could consider making
;; this harder to press, instead making my-company-complete-with-dabbrev easier.
(global-set-key (kbd "M-SPC") 'company-complete)

;; When LSP is enabled, completion is done via the language server. This is often nice,
;; but sometimes I just want to be able to do dumb completion using available symbols. So
;; here's a function that will allow that when it would be helpful.
;;
;; I considered changing the backends list so that both capf and dabbrev-code are used at
;; once, probably distinguished using the company-box extension. The problem with that,
;; though, is that they operate differently in terms of fuzzy matching, so I felt like
;; mixing them could be weird and hard to use. But I could see maybe doing that instead of
;; (or in addition to?) having this separate function.
(defun my-company-complete-with-dabbrev ()
  (interactive)
  ;; I also wanted to use a different company-tooltip face for this (e.g., background
  ;; azure), to make it more visually obvious that I am doing something different, but I
  ;; can't see an easy way to accomplish that: it sounds like you can't let-bind a face.
  (let ((company-backends '(company-dabbrev-code company-dabbrev)))
    (company-complete))
  )
(global-set-key (kbd "A-M-SPC") 'my-company-complete-with-dabbrev)

;; This is useful for turning off the company idle delay in buffers where this causes a slowdown (e.g., case.py)
(defun my-toggle-company-idle-delay ()
  (interactive)
  (if (null company-idle-delay)
      (setq-local company-idle-delay 0.15)
    (setq-local company-idle-delay nil))
  (message "company-idle-delay set to %s" company-idle-delay))

;; Sometimes I just want to insert a newline, not have company pick the
;; current selection. Having return bound to company-complete-selection
;; is especially problematic when the top candidate is (currently typed
;; text) + (some suffix); this can particularly happen when candidates
;; are sorted by occurrence (or anything other than alphabetically, I
;; guess). To get around this, use M-SPC rather than Ret for
;; company-complete-selection. I have chosen this binding to be similar to the binding I
;; set above for company-complete.
;;
;; (See also https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil)
  ;; Make both M-SPC and A-M-SPC run company-complete-selection. This feels easiest
  ;; because I sometimes trigger company with M-SPC and sometimes with A-M-SPC (the latter
  ;; for completing with dabbrev); when triggering with the latter, I naturally want to
  ;; press the same thing to complete with the current selection because my fingers are
  ;; already there.
  (define-key company-active-map (kbd "M-SPC") 'company-complete-selection)
  (define-key company-active-map (kbd "A-M-SPC") 'company-complete-selection)
  (define-key company-active-map (kbd "A-SPC") 'company-complete-common)
  ;; C-g will quit, but it could be helpful to have an alternative way to cancel the completion, too
  (define-key company-active-map (kbd "S-SPC") 'company-abort)

  ;; Do not try to auto-complete comments or plain text
  ;; need to customize company-backends this way rather than via standard customization so
  ;; that we avoid overriding backends added by prelude (e.g., anaconda)
  (setq company-backends (remove 'company-dabbrev company-backends))
  )
