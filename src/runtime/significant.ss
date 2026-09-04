;;; -*- Gerbil -*-
;;; Runtime-only projection from lossless tokens to parser-significant tokens.

(import ../compiler/machine)
(export parser-significant-tokens)

(def (parser-significant-tokens machine tokens)
  (let loop ((rest tokens) (found '()))
    (cond
     ((null? rest) (reverse found))
     (((parser-machine-trivia machine) (car rest))
      (loop (cdr rest) found))
     (else (loop (cdr rest) (cons (car rest) found))))))
