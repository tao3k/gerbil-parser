(import (only-in :gerbil-parser/src/runtime/lr-parser/state
                 lr-runtime-reduction-widths make-lr-recognition-fragment)
        (only-in :gerbil-parser/src/runtime/lr-parser/recognition
                 recognition-piece-start recognition-piece-end recognition-piece-token-count
                 recognition-piece-terminal-yield terminal-yield-append)
        (only-in :gerbil-parser/src/runtime/token token-kind token-lexeme))
(export reference-capture-reduction)
;;; Frozen capture control; it does not select or replace a runtime binding.
(def (reference-capture-reduction runtime executor production-id states stack rest offset value target)
  (if (not stack) #f
    (let* ((count (vector-ref (lr-runtime-reduction-widths runtime) production-id))
           (children (reverse (take stack count)))
           (remaining (drop stack count))
           (entry-state (list-ref states count))
           (lookahead (if (pair? rest)
                        (cons (token-kind (car rest)) (token-lexeme (car rest)))
                        #f))
           (node (make-lr-recognition-fragment
                  runtime executor production-id entry-state target children lookahead value
                  (if (pair? children) (recognition-piece-start (car children)) offset)
                  (if (pair? children) (recognition-piece-end (last children)) offset)
                  (foldl (lambda (piece count)
                           (+ count (recognition-piece-token-count piece)))
                         0 children) offset
                  (foldl (lambda (piece yield)
                           (terminal-yield-append yield (recognition-piece-terminal-yield piece)))
                         #f children))))
      (cons node remaining))))
