;;; -*- Gerbil -*-
;;; Shared TLA+ lexical expression data; no version or source metadata.
(import (only-in :gerbil-parser/language-support/grammar deftext-profile))
(export tla-proof-name tla-proof-start tla-proof-reference tla-identifier)
(deftext-profile tla-proof-name
  (seq (literal "<") (if-next (characters "+*") (run (characters "+*") 1 1) (run (numeric) 1 #f))
    (literal ">")
    (if-next (union (alphabetic) (numeric) (characters "_"))
      (run (union (alphabetic) (numeric) (characters "_")) 1 #f)
      (optional (run (characters "*-") 1 1))) (run (characters ".") 0 #f)))
(deftext-profile tla-proof-start
  (ends-not-in (union (alphabetic) (numeric) (characters "_")) (ref tla-proof-name)))
(deftext-profile tla-proof-reference
  (ends-in (union (alphabetic) (numeric) (characters "_")) (ref tla-proof-name)))
(deftext-profile tla-identifier
  (run-containing (union (alphabetic) (numeric) (characters "_"))
    (union (alphabetic) (characters "_")) 1 #f))
