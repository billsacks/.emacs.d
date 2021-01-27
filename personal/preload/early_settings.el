(eval-when-compile
  (require 'use-package))

(setq winner-dont-bind-my-keys t)

;; this needs to be set before the (require 'tramp) done in prelude
(add-to-list 'password-word-equivalents "Token_Response")
(add-to-list 'password-word-equivalents "TokenResponse")
