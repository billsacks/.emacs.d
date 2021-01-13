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
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 4)
 '(company-transformers '(company-sort-by-occurrence))
 '(cperl-continued-statement-offset 3)
 '(cperl-indent-level 3)
 '(cperl-label-offset -3)
 '(cursor-type 'bar)
 '(ediff-split-window-function 'split-window-horizontally)
 '(f90-associate-indent 0)
 '(f90-beginning-ampersand nil)
 '(f90-break-before-delimiters nil)
 '(f90-program-indent 2)
 '(fill-column 72)
 '(find-grep-options "-q -i")
 '(fit-window-to-buffer-horizontally t)
 '(flycheck-idle-change-delay 5)
 '(flycheck-indication-mode 'left-margin)
 '(flycheck-mode-line-prefix "FC")
 '(fortran-do-indent 2)
 '(fortran-if-indent 2)
 '(fortran-line-length 131)
 '(fortran-structure-indent 2)
 '(frame-resize-pixelwise t)
 '(global-nlinum-mode nil)
 '(global-tab-line-mode t)
 '(hl-todo-keyword-faces
   '(("TODO" . "Firebrick")
     ("KLUDGE" . "Firebrick")
     ("HACK" . "Firebrick")
     ("TEMP" . "Firebrick")
     ("FIXME" . "Firebrick")))
 '(ivy-extra-directories nil)
 '(ivy-height 16)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'abbreviate)
 '(lsp-enable-imenu nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-imenu-sort-methods '(kind position))
 '(lsp-ui-doc-position 'at-point)
 '(magit-ediff-dwim-show-on-hunks t)
 '(major-mode 'text-mode)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position smartrep-mode-line-string "  " mode-line-modes mode-line-misc-info
     (vc-mode vc-mode)
     mode-line-end-spaces))
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(nxml-attribute-indent 0)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 3)
 '(org-agenda-files '("~/notes"))
 '(org-agenda-window-setup 'current-window)
 '(package-selected-packages
   '(rg visual-fill-column yasnippet lsp-python-ms cmake-mode projectile-ripgrep yaml-mode web-mode lsp-ui company-lsp json-mode js2-mode rainbow-mode elisp-slime-nav rainbow-delimiters company counsel swiper ivy exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number nlinum move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring anzu ag ace-window))
 '(prelude-clean-whitespace-on-save nil)
 '(prelude-guru nil)
 '(projectile-mode-line-prefix " P")
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "ipython")
 '(rg-command-line-flags '("--sort path"))
 '(rg-keymap-prefix [8388723])
 '(ripgrep-arguments '("--sort path"))
 '(size-indication-mode nil)
 '(tool-bar-mode nil)
 '(vc-git-grep-template
   "git --no-pager grep --show-function --color -n <C> -e <R> -- <F>")
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
 '(ediff-fine-diff-C ((t (:background "#f0f055"))))
 '(font-lock-builtin-face ((t (:foreground "#6052a3"))))
 '(font-lock-keyword-face ((t (:foreground "#932092"))))
 '(hl-line ((t (:inherit highlight :extend t :background "#e2eee2"))))
 '(hl-todo ((t (:inherit font-lock-comment-face :weight bold))))
 '(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))
 '(magit-section-highlight ((t (:extend t :background "grey92"))))
 '(match ((t (:foreground "DarkOrange2" :weight bold))))
 '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button) :height 0.8 :family "Lucida Grande"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75")))))
 '(rg-match-face ((t (:foreground "dark red" :weight bold))))
 '(secondary-selection ((t (:extend t :background "gold1"))))
 '(shadow ((t (:foreground "SteelBlue3"))))
 '(whitespace-trailing ((t (:background "LightYellow" :foreground "lightgray" :weight bold)))))
