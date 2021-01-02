;; disable some things loaded by default by prelude

;; I have no need for this
(editorconfig-mode 0)

;; I don't like the aggressive saving of buffers
(super-save-mode 0)

;; Undo the definition of this function (otherwise auto-fill only
;; happens in comments; I don't want that behavior)
(defun prelude-local-comment-auto-fill ()
  )
