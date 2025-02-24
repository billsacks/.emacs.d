;;; prelude-mode.el --- Emacs Prelude: minor mode
;;
;; Copyright © 2011-2021 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode defining a local keymap, plus a menu.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'easymenu)
(require 'imenu-anywhere)
(require 'crux)
(require 'browse-at-remote)

(defvar prelude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-/ g") 'prelude-google)
    (define-key map (kbd "C-c C-/ h") 'prelude-github)
    (define-key map (kbd "C-c C-/ y") 'prelude-youtube)
    (define-key map (kbd "C-c C-/ d") 'prelude-duckduckgo)
    (define-key map (kbd "M-o") 'crux-smart-open-line)
    (define-key map (kbd "C-c u") 'crux-view-url)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    ;; extra prefix for projectile
    (when prelude-super-keybindings
     (define-key map (kbd "s-p") 'projectile-command-map))
    ;; make some use of the Super key
    (when prelude-super-keybindings
      ;; crux
      (define-key map (kbd "s-j") 'crux-top-join-line)
      (define-key map (kbd "s-k") 'crux-kill-whole-line)
      ;; magit
      (define-key map (kbd "s-m s") 'my-magit-status-magit-frame)
      (define-key map (kbd "s-m d") 'magit-dispatch)
      (define-key map (kbd "s-m f") 'magit-file-dispatch)
      (define-key map (kbd "s-m l") 'magit-log-buffer-file)
      (define-key map (kbd "s-m b") 'magit-blame)
      (define-key map (kbd "s-m y") 'my-magit-show-local-refs)
      (define-key map (kbd "s-m m") 'my-magit-get-merge-base)
      (define-key map (kbd "s-m r") 'browse-at-remote)
      ;; misc
      (define-key map (kbd "s-/") 'hippie-expand))
    (easy-menu-define prelude-mode-menu map
      "Prelude's menu."
      '("Prelude"
        ("Files"
         ["Open with..." crux-open-with]
         ["Re-open as root" crux-reopen-as-root]
         ["Delete file and buffer" crux-delete-file-and-buffer]
         ["Rename buffer and file" crux-rename-buffer-and-file]
         ["Find init file" crux-find-user-init-file]
         ["Find custom file" crux-find-user-custom-file]
         ["Find shell config file" crux-find-shell-init-file])
        ("Buffers"
         ["Clean up buffer or region" crux-cleanup-buffer-or-region]
         ["Kill other buffers" crux-kill-other-buffers])
        ("Editing"
         ["Go to beginning of line" crux-move-beginning-of-line]
         ["Kill line" crux-smart-kill-line]
         ["Kill whole line" crux-kill-whole-line]
         ["Insert empty line below" crux-smart-open-line]
         ["Insert empty line above" crux-smart-open-line-above]
         ["Move up" move-text-up]
         ["Move down" move-text-down]
         ["Duplicate line or region" crux-duplicate-current-line-or-region]
         ["Indent rigidly and copy to clipboard" crux-indent-rigidly-and-copy-to-clipboard]
         ["Indent defun" crux-indent-defun]
         ["Insert date" crux-insert-date]
         ["Eval and replace" crux-eval-and-replace])
        ("Windows"
         ["Swap windows" crux-swap-windows])
        ("General"
         ["Visit term buffer" crux-visit-term-buffer]
         ["Search in Google" prelude-google]
         ["View URL" crux-view-url])))
    map)
  "Keymap for Prelude mode.")

;; define minor mode
(define-minor-mode prelude-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{prelude-mode-map}"
  :lighter " Pre"
  :keymap prelude-mode-map
  :global t)

(provide 'prelude-mode)
;;; prelude-mode.el ends here
