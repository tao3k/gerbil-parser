;;; -*- Gerbil -*-
;;; Record assignments corpus manifest embedded at macro expansion.

(import (only-in :gerbil-parser/language-support defsyntax-corpus)
        (only-in ./grammar
                 +records-language-version+
                 +records-syntax-contract+))
(export records-fixtures)

(defsyntax-corpus records-fixtures
  (identity "record-assignments"
            +records-language-version+
            +records-syntax-contract+)
  (accepted
   ("records/accepted" records-accepted "corpus/accepted.records"
    Document (Assignment)))
  (rejected
   ("records/rejected" records-rejected "corpus/rejected.records")))
