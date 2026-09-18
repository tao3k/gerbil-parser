;;; -*- Gerbil -*-
;;; Downstream corpus manifest embedded at macro expansion.

(import (only-in :gerbil-parser/language-support defsyntax-corpus)
        (only-in ./grammar
                 +records-language-version+
                 +records-syntax-contract+))
(export records-v1-fixtures)

(defsyntax-corpus records-v1-fixtures
  (identity "downstream-records"
            +records-language-version+
            +records-syntax-contract+)
  (accepted
   ("records/accepted" records-v1-accepted "corpus/accepted.records"
    Document (Assignment)))
  (rejected
   ("records/rejected" records-v1-rejected "corpus/rejected.records")))
