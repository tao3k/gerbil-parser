;;; -*- Gerbil -*-
;;; Ordinary Scheme validators, imported at the transformer's phase.
(import (only-in :gerbil/expander identifier? raise-syntax-error))
(export require-declaration-identifier)
(def (require-declaration-identifier term declaration operation parameter (author #f))
  (unless (identifier? term)
    (let (message (string-append operation ": " parameter " must be an identifier"))
      (if author
        (raise-syntax-error #f message term author declaration)
        (raise-syntax-error #f message term declaration)))))
