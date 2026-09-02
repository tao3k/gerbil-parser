;;; Tight parser-specific functions shared by generated machines.

(import ../../compiler/normalize
        ../../compiler/parser-ir
        ../../compiler/generate
        ../../runtime/cst
        ../../runtime/token
        ../../runtime/lexer)
(export (import: ../../compiler/normalize)
        (import: ../../compiler/parser-ir)
        (import: ../../compiler/generate)
        (import: ../../runtime/cst)
        (import: ../../runtime/token)
        (import: ../../runtime/lexer)
        parser-significant-tokens)

(def (parser-significant-tokens machine tokens)
  (let loop ((rest tokens) (found '()))
    (cond
     ((null? rest) (reverse found))
     (((parser-machine-trivia machine) (car rest))
      (loop (cdr rest) found))
     (else (loop (cdr rest) (cons (car rest) found))))))
