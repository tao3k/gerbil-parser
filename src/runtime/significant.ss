;;; -*- Gerbil -*-
;;; Runtime-only projection from lossless tokens to parser-significant tokens.

(import (only-in ../compiler/machine parser-machine-trivia))
(export parser-significant-tokens parser-significant-joined)

;; : (-> ParserMachine (List Token) (List Token))
(def (parser-significant-tokens machine tokens)
  (let (trivia? (parser-machine-trivia machine))
    (filter (lambda (token) (not (trivia? token))) tokens)))

;; A fork has a reversed deterministic prefix and a fresh forward suffix.
;; Filter the suffix once, then prepend significant prefix tokens in order;
;; `rest` stays an exact tail of the combined significant stream for GLR.
(def (parser-significant-joined machine prefix-reversed suffix)
  (let* ((trivia? (parser-machine-trivia machine))
         (rest (filter (lambda (token) (not (trivia? token))) suffix)))
    (values
     (foldl (lambda (token combined)
              (if (trivia? token) combined (cons token combined)))
            rest prefix-reversed)
     rest)))
