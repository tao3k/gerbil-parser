;;; -*- Gerbil -*-
;;; Runtime-only projection from lossless tokens to parser-significant tokens.

(import (only-in :std/sugar filter)
        (only-in ../compiler/machine parser-machine-trivia))
(export parser-significant-tokens)

;; : (-> ParserMachine (List Token) (List Token))
(def (parser-significant-tokens machine tokens)
  (let (trivia? (parser-machine-trivia machine))
    (filter (lambda (token) (not (trivia? token))) tokens)))
