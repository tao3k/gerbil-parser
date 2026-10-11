;;; -*- Gerbil -*-
(import (only-in ../../src/language/grammar defgrammar-syntax))
(export required-items broken-wrapper)
(defgrammar-syntax (required-items label item separator)
  (seq (field label item)
       (repeat (seq separator (field label item)))))

;; A library template with an invalid constructor must blame its author call.
(defgrammar-syntax (broken-wrapper item)
  (misspelled item))
