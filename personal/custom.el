(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-interval 0)
 '(avy-keys '(97 111 101 117 105 100 104 116 110 115))
 '(blink-cursor-blinks 2)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(company-abort-manual-when-too-short t)
 '(company-dabbrev-char-regexp "[[:word:]_]")
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay nil)
 '(company-minimum-prefix-length 1)
 '(cperl-continued-statement-offset 3)
 '(cperl-indent-level 3)
 '(cperl-label-offset -3)
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(deft-auto-save-interval 300.0)
 '(deft-directory "/Users/sacks/notes")
 '(deft-extensions '("org"))
 '(deft-file-naming-rules '((noslash . "-") (nospace . "-") (case-fn . downcase)))
 '(deft-org-mode-title-prefix nil)
 '(deft-use-filter-string-for-filename t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(edit-server-new-frame nil)
 '(edit-server-new-frame-alist
   '((name . "Edit with Emacs FRAME")
     (width . 132)
     (minibuffer . t)
     (menu-bar-lines . t)
     (fullscreen . fullheight)
     (left . 960)))
 '(edit-server-start-hook '(my-document-mode-changes))
 '(edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode)))
 '(f90-associate-indent 0)
 '(f90-beginning-ampersand nil)
 '(f90-break-before-delimiters nil)
 '(f90-program-indent 2)
 '(fill-column 72)
 '(find-grep-options "-q -i")
 '(flycheck-color-mode-line-face-to-color 'sml/modes)
 '(flycheck-color-mode-line-show-running nil)
 '(flycheck-display-errors-function 'ignore)
 '(flycheck-gfortran-language-standard "f2008")
 '(flycheck-highlighting-mode nil)
 '(flycheck-indication-mode 'left-margin)
 '(flycheck-mode-line '(:eval (my-flycheck-mode-line-status-text)))
 '(flycheck-mode-line-prefix "FC")
 '(fortran-do-indent 2)
 '(fortran-if-indent 2)
 '(fortran-line-length 131)
 '(fortran-structure-indent 2)
 '(frame-resize-pixelwise t)
 '(global-tab-line-mode t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(highlight-indent-guides-method 'bitmap)
 '(highlight-indent-guides-responsive nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "Firebrick")
     ("KLUDGE" . "Firebrick")
     ("HACK" . "Firebrick")
     ("TEMP" . "Firebrick")
     ("FIXME" . "Firebrick")))
 '(isearch-allow-scroll 'unlimited)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-extra-directories nil)
 '(ivy-height 16)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'abbreviate)
 '(line-move-visual nil)
 '(lsp-auto-execute-action nil)
 '(lsp-clients-fortls-args '("--lowercase_intrinsics"))
 '(lsp-diagnostics-attributes
   '((unnecessary :foreground "gray40")
     (deprecated :strike-through t)))
 '(lsp-diagnostics-disabled-modes '(f90-mode))
 '(lsp-eldoc-enable-hover nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-imenu nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-imenu-sort-methods '(kind position))
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-modeline-code-actions-segments '(count))
 '(lsp-modeline-diagnostics-enable nil)
 '(lsp-modeline-diagnostics-scope :file)
 '(lsp-progress-function 'ignore)
 '(lsp-pyright-auto-import-completions nil)
 '(lsp-pyright-extra-paths ["./python" "./cime/scripts/lib" "./scripts/lib"])
 '(lsp-pyright-typechecking-mode "off")
 '(lsp-signature-auto-activate '(:on-server-request))
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-position 'at-point)
 '(magit-diff-refine-hunk 'all)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-git-executable "/usr/local/bin/git")
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 10))
 '(magit-refs-margin '(t age-abbreviated magit-log-margin-width t 10))
 '(magit-section-initial-visibility-alist
   '((stashes . hide)
     ([remote branchbuf]
      . hide)
     ([tags branchbuf]
      . hide)))
 '(magit-status-margin '(nil "%Y-%m-%d %H:%M " magit-log-margin-width t 10))
 '(major-mode 'text-mode)
 '(markdown-asymmetric-header t)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("fortran" . f90-mode)))
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-header-scaling nil)
 '(markdown-header-scaling-values '(1.5 1.3 1.2 1.1 1.0 1.0))
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position smartrep-mode-line-string "  " mode-line-modes mode-line-misc-info
     (vc-mode vc-mode)
     mode-line-end-spaces))
 '(mouse-drag-copy-region t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(next-screen-context-lines 3)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(nxml-attribute-indent 0)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 3)
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/notes"))
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c%l%l")
     (tags . " %i %-12:c%l%l")
     (search . " %i %-12:c%l%l")))
 '(org-agenda-window-setup 'current-window)
 '(org-babel-load-languages '((emacs-lisp . t) (python . t)))
 '(org-blank-before-new-entry '((heading) (plain-list-item . auto)))
 '(org-cycle-emulate-tab nil)
 '(org-edit-src-content-indentation 0)
 '(org-ellipsis "...")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-section-numbers nil)
 '(org-export-with-sub-superscripts '{})
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers t)
 '(org-imenu-depth 3)
 '(org-inlinetask-default-state "TODO")
 '(org-list-indent-offset 2)
 '(org-src-window-setup 'current-window)
 '(org-startup-folded 'content)
 '(org-startup-indented t)
 '(org-tag-alist
   '(("cesm_general")
     ("cism")
     ("cism_multiple_icesheets")
     ("ctsm")
     ("ncar")))
 '(org-use-fast-todo-selection 'expert)
 '(org-use-sub-superscripts '{})
 '(org-variable-pitch-fixed-faces
   '(org-block org-code org-document-info-keyword org-done org-formula org-indent org-meta-line org-special-keyword org-table org-todo org-verbatim org-date org-drawer))
 '(package-selected-packages
   '(org-sidebar org-variable-pitch org-appear edit-server flycheck-color-mode-line smart-mode-line lsp-pyright realgud ox-gfm deft lsp-treemacs highlight-indent-guides helm-projectile helm ws-butler use-package browse-at-remote ibuffer-vc adaptive-wrap rg visual-fill-column yasnippet cmake-mode projectile-ripgrep yaml-mode web-mode lsp-ui json-mode js2-mode rainbow-mode elisp-slime-nav rainbow-delimiters company counsel swiper ivy exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number nlinum move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring anzu ag ace-window))
 '(prelude-clean-whitespace-on-save nil)
 '(prelude-guru nil)
 '(prelude-whitespace nil)
 '(projectile-dynamic-mode-line nil)
 '(projectile-mode-line-prefix " P")
 '(projectile-switch-project-action 'projectile-commander)
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "ipython")
 '(rainbow-delimiters-max-face-count 4)
 '(rg-command-line-flags '("--sort path"))
 '(rg-keymap-prefix [8388723])
 '(ripgrep-arguments '("--sort path"))
 '(save-interprogram-paste-before-kill t)
 '(sentence-end-double-space nil)
 '(size-indication-mode nil)
 '(sml/col-number-format "%3c")
 '(sml/full-mode-string " <")
 '(sml/line-number-format "%4l")
 '(sml/modified-char "x")
 '(sml/position-percentage-format nil)
 '(sml/shorten-mode-string " >")
 '(sml/shorten-modes nil)
 '(sp-base-key-bindings nil)
 '(sp-highlight-pair-overlay nil)
 '(tab-line-exclude-modes '(completion-list-mode ediff-mode))
 '(tool-bar-mode nil)
 '(vc-git-grep-template
   "git --no-pager grep --show-function --ignore-case --color -n <C> -e <R> -- <F>")
 '(visual-fill-column-fringes-outside-margins nil)
 '(whitespace-style '(face trailing tabs empty))
 '(yas-also-auto-indent-first-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#f6f6f6" :foreground "#333333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Hack"))))
 '(anzu-mode-line ((t (:foreground "royal blue" :weight bold))))
 '(cursor ((t (:background "#933141"))))
 '(diff-hl-change ((t (:background "#eeeeff" :foreground "#8888ff"))))
 '(diff-hl-delete ((t (:background "#ffeeee" :foreground "#ff8888"))))
 '(diff-hl-insert ((t (:background "#eeffee" :foreground "#88bb88"))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "#66ff66"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#ffaaaa"))))
 '(ediff-current-diff-A ((t (:extend t :background "#e3f2ff"))))
 '(ediff-current-diff-Ancestor ((t (:extend t :background "#e3f2ff"))))
 '(ediff-current-diff-B ((t (:extend t :background "#e3f2ff"))))
 '(ediff-current-diff-C ((t (:extend t :background "#e3f2ff"))))
 '(ediff-even-diff-A ((t (:extend t :background "#e8dfe0"))))
 '(ediff-even-diff-Ancestor ((t (:extend t :background "#e8dfe0"))))
 '(ediff-even-diff-B ((t (:extend t :background "#e8dfe0"))))
 '(ediff-even-diff-C ((t (:extend t :background "#e8dfe0"))))
 '(ediff-fine-diff-A ((t (:background "#bfe2ff"))))
 '(ediff-fine-diff-Ancestor ((t (:background "#bfe2ff"))))
 '(ediff-fine-diff-B ((t (:background "#bfe2ff"))))
 '(ediff-fine-diff-C ((t (:background "#bfe2ff"))))
 '(ediff-odd-diff-A ((t (:extend t :background "#e3e6e4"))))
 '(ediff-odd-diff-Ancestor ((t (:extend t :background "#e3e6e4"))))
 '(ediff-odd-diff-B ((t (:extend t :background "#e3e6e4"))))
 '(ediff-odd-diff-C ((t (:extend t :background "#e3e6e4"))))
 '(fixed-pitch ((t (:family "Hack"))))
 '(flycheck-color-mode-line-error-face ((t (:foreground "DarkRed"))))
 '(flycheck-color-mode-line-info-face ((t (:inherit flycheck-color-mode-line-success-face))))
 '(flycheck-color-mode-line-success-face ((t (:foreground "gray50"))))
 '(flycheck-color-mode-line-warning-face ((t (:foreground "DarkOrange4"))))
 '(flycheck-error ((t (:underline "#ffccee"))))
 '(flycheck-fringe-error ((t (:foreground "#ffccee"))))
 '(flycheck-fringe-info ((t (:foreground "gray85"))))
 '(flycheck-fringe-warning ((t (:foreground "#ffd498"))))
 '(flycheck-info ((t (:underline "gray85"))))
 '(flycheck-warning ((t (:underline "#ffd498"))))
 '(flyspell-duplicate ((t (:underline (:color "DarkSlateBlue" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "LightSlateBlue" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "#6052a3"))))
 '(font-lock-keyword-face ((t (:foreground "#932092"))))
 '(hi-black-b ((t (:foreground "#333333" :inverse-video t))))
 '(hi-black-hb ((t (:foreground "#888888" :inverse-video t))))
 '(hi-blue-b ((t (:foreground "blue1" :inverse-video t))))
 '(hi-green-b ((t (:foreground "green3" :inverse-video t))))
 '(hi-red-b ((t (:foreground "firebrick2" :inverse-video t))))
 '(hi-yellow ((t (:background "gold1"))))
 '(hl-line ((t (:inherit highlight :extend t :background "#e9f2e9"))))
 '(hl-todo ((t (:inherit font-lock-comment-face :weight bold))))
 '(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))
 '(magit-diff-added ((t (:extend t :background "#ddffdd" :foreground "#333333"))))
 '(magit-diff-added-highlight ((t (:extend t :background "#cceecc" :foreground "#333333"))))
 '(magit-diff-removed ((t (:extend t :background "#ffdddd" :foreground "#333333"))))
 '(magit-diff-removed-highlight ((t (:extend t :background "#eecccc" :foreground "#333333"))))
 '(magit-section-highlight ((t (:extend t :background "grey92"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#efefef" :height 120))))
 '(markdown-gfm-checkbox-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :weight bold :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :weight bold :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :weight bold :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :weight bold :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :weight bold :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :weight bold :height 1.0))))
 '(match ((t (:background "gold1"))))
 '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button) :height 0.8 :family "Lucida Grande"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75")))))
 '(org-block ((t (:extend t :foreground "#333333"))))
 '(org-code ((t (:inherit font-lock-constant-face))))
 '(org-ellipsis ((t (:background "LightYellow2" :foreground "DarkGoldenrod" :underline t))))
 '(org-inlinetask ((t (:inherit font-lock-function-name-face))))
 '(org-quote ((t (:inherit default :foreground "dark blue"))))
 '(org-variable-pitch-fixed-face ((t (:height 0.8 :family "Hack"))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "red1" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#6052a3"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ab5300"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "ForestGreen"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "VioletRed4"))))
 '(region ((t (:extend t :background "#f6e696" :distant-foreground "#333333"))))
 '(rg-match-face ((t (:foreground "darkred" :weight bold))))
 '(rst-literal ((t (:inherit (fixed-pitch font-lock-constant-face) :background "#efefef" :height 120))))
 '(secondary-selection ((t (:extend t :background "#fff9d6"))))
 '(show-paren-match ((t (:background "#d4bfff"))))
 '(sml/col-number ((t (:inherit sml/global :height 1.1 :family "Hack"))))
 '(sml/line-number ((t (:inherit sml/global :weight bold :height 1.1 :family "Hack"))))
 '(sml/minor-modes ((t (:inherit sml/modes))))
 '(sml/modes ((t (:inherit sml/global :foreground "Black" :height 0.9 :family "Hack"))))
 '(sml/not-modified ((t (:inherit sml/global :height 1.1 :family "Hack"))))
 '(sml/position-percentage ((t (:inherit sml/prefix :weight normal :height 1.1 :family "Hack"))))
 '(sml/read-only ((t (:inherit sml/global :foreground "DarkGreen" :weight bold))))
 '(sml/time ((t (:inherit sml/global))))
 '(tab-line ((t (:background "grey85" :foreground "black" :height 0.85 :family "Lucida Grande"))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "wheat"))))
 '(variable-pitch ((t (:height 1.25 :family "Charter"))))
 '(whitespace-empty ((t (:background "LightYellow" :foreground "firebrick"))))
 '(whitespace-trailing ((t (:background "LightYellow" :foreground "lightgray" :weight bold)))))
